# The purpose of this script is to simulate a season/instance of a survivor pool
# with n-players.

# Process
#   - Sample pick data
#   - Check picks against actual results
#   - Extract pool pick feature data
#       - Weekly summary of player status
#       - Weekly summary of player picks remaining
#   - Extract winners/results (ROI, Max Week)

# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))

# Setup parallel processing
plan(multisession, workers = 6)


# LOAD_DATA ---------------------------------------------------------------

# Load databrames
df_results_all <- read_parquet(here("data", "prepared", "df_results.parquet"))
df_teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# Extract team short ids
team_ids <- df_teams %>% 
  pull(team_master_short) %>% 
  unique() %>% 
  sort() %>%
  str_to_lower()


# DEFINE SIMULATION FUNCTION ----------------------------------------------
 
simulate_pool_season <- function(season, data_pool_picks, data_results, n_pool_players) {
  
  # Sample from picks to generate sample season
  df_pool_season <- data_pool_picks %>%
    slice_sample(n = n_pool_players, replace = TRUE) %>%
    mutate(
      player_id = 1:n_pool_players %>% str_pad(width = 3, side = "left", pad = "0"),
      data = map(path, read_parquet)
    ) %>%
    unnest(data) %>%
    select(player_id, pick_hash, week, pick)
  
  # Check results
  df_pool_season <- df_pool_season %>%
    left_join(data_results, by = c("week", "pick" = "team")) %>%
    mutate(is_correct = (result %in% c("W", "T"))) %>%
    group_by(player_id) %>%
    arrange(week) %>%
    mutate(
      count_deaths = cumsum(!is_correct),
      is_alive = count_deaths == 0,
      is_alive = lag(is_alive, 1)
    ) %>%
    replace_na(list(is_alive = TRUE)) %>%
    mutate(
      pick = ifelse(is_alive, pick, NA),
      is_correct = ifelse(is_alive, is_correct, NA)
    ) %>%
    ungroup() %>%
    arrange(player_id) %>%
    select(player_id, pick_hash, week, pick, is_correct, is_alive)
  
  # Gather feature data - teams remaining to each player by week
  df_teams_remaining <- df_pool_season %>%
    drop_na(pick) %>%
    mutate(
      pick = str_to_lower(pick),
      pick_status = TRUE
    ) %>%
    arrange(pick) %>%
    pivot_wider(
      id_cols = c(player_id, week),
      names_from = pick,
      names_prefix = "is_available_",
      values_from = pick_status
    ) %>%
    arrange(player_id, week) %>%
    group_by(player_id) %>%
    mutate(across(starts_with("is_available_"), ~!is.na(.))) %>%
    mutate(across(starts_with("is_available_"), cumsum)) %>%
    mutate(across(starts_with("is_available_"), cumsum)) %>%
    mutate(across(starts_with("is_available_"), ~ . <= 1)) %>%
    ungroup()
  
  # Add missing teams (i.e. can happen if there were teams that no one picked)
  for (team in team_ids) {
    col <- str_flatten(c("is_available_", team))
    if (! col %in% names(df_teams_remaining)) {
      df_teams_remaining[[col]] <- TRUE
    }
  }
  df_teams_remaining <- df_teams_remaining %>%
    select(player_id, week, sort(colnames(.)))
  
  # TODO: Extract player status by week
  
  # Extract top player results
  df_pool_top <- df_pool_season %>%
    filter(is_alive) %>%
    filter(week == max(week)) %>%
    mutate(correct_picks = week - 1) %>%
    select(player_id, pick_hash, correct_picks)
  
  # Generate file name
  file_name <- df_pool_season %>%
    pull(pick_hash) %>%
    unique() %>%
    str_flatten() %>%
    md5() %>%
    str_sub(1, 8) %>%
    str_c(".parquet")
  print(str_flatten(c("Completed Simulation: ", file_name)))
  
  # Generate path/folder
  dir.create(here("data", "sims_pool_seasons", "results", season), recursive = TRUE, showWarnings = FALSE)
  dir.create(here("data", "sims_pool_seasons", "teams_remaining", season), recursive = TRUE, showWarnings = FALSE)
  
  # Write to disk results
  write_parquet(df_pool_top, here("data", "sims_pool_seasons", "results", season, file_name))
  write_parquet(df_teams_remaining, here("data", "sims_pool_seasons", "teams_remaining", season, file_name))
  
  return(NULL)
}


# GENERATE SIMULATION SAMPLES ---------------------------------------------

# Gather collection of available pick samples
df_pool_picks_all <- 
  list.files(here("data", "sims_pool_picks"), recursive = TRUE, full.names = TRUE) %>%
  enframe("idx", "path") %>%
  mutate(
    season = str_extract(path, "[:digit:]{4}(?=\\/[:alnum:]{8}.parquet)") %>% as.numeric(),
    pick_hash = str_extract(path, "[:alnum:]{8}(?=.parquet)")
  ) %>%
  select(season, pick_hash, path)

# Create grid of results for each season
grid_results <- df_results_all %>% 
  group_by(season) %>%
  nest(.key = "data_results") %>%
  ungroup()

# Run simulations
df_pool_picks_all %>%
  filter(
    season >= params$year_min,
    ! season %in% params$years_exclude,
  ) %>%
  group_by(season) %>%
  nest(.key = "data_pool_picks") %>%
  ungroup()
  left_join(grid_results, by = "season") %>%
  mutate(sim_id = list(1:params$n_pool_season_sims)) %>%
  unnest(sim_id) %>%
  mutate(
    run = future_pmap(
      list(season, data_pool_picks, data_results), 
      simulate_pool_season,
      n_pool_players = params$n_pool_players,
      .progress = TRUE
    )
  )





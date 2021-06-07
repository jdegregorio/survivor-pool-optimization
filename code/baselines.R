# The purpose of this script is to evaluate baseline performance measures for
# survivor pools.

# Baseline Strategies:
#  - Naive:  Random(ish) pick selection based on pick distributions
#  - Group-Think:  Pick the available team with the greatest weekly pick distribution
#  - Expected Value: Pick the available team with the greatest expected value


# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))

# Setup parallel processing
plan(multisession, workers = 6)

# Load results data
df_results_all <- read_parquet(here("data", "prepared", "df_results.parquet"))

# SAMPLE SEASON SIMULATIONS -----------------------------------------------

# Load simulation results
df_sim_results <- 
  list.files(here("data", "sims_pool_seasons", "results"), recursive = TRUE, full.names = TRUE) %>%
  enframe("idx", "path") %>%
  mutate(
    season = str_extract(path, "[:digit:]{4}(?=\\/[:alnum:]{8}.parquet)") %>% as.numeric(),
    season_hash = str_extract(path, "[:alnum:]{8}(?=.parquet)")
  ) %>%
  select(season, season_hash, path) %>%
  group_by(season) %>%
  slice_sample(n = params$baseline_samples) %>%
  ungroup() %>%
  mutate(data_top = future_map(path, read_parquet))

# Extract simulation results
df_sim_results <- df_sim_results %>%
  mutate(
    top_n = map_dbl(data_top, nrow),
    top_correct = map_dbl(data_top, ~ .x %>% pull(correct_picks) %>% max())
  ) %>%
  select(season, season_hash, top_n, top_correct)

# Create grid of actual game results for each season
grid_results <- df_results_all %>% 
  group_by(season) %>%
  nest(data_results = c(week, team, result)) %>%
  ungroup()


# BASELINE - RANDOM PICK DISTRIBUTION -------------------------------------

# Gather collection of available pick samples
grid_pool_picks <- 
  list.files(here("data", "sims_pool_picks"), recursive = TRUE, full.names = TRUE) %>%
  enframe("idx", "path") %>%
  mutate(
    season = str_extract(path, "[:digit:]{4}(?=\\/[:alnum:]{8}.parquet)") %>% as.numeric(),
    pick_hash = str_extract(path, "[:alnum:]{8}(?=.parquet)")
  ) %>%
  select(season, pick_hash, path) %>%
  group_by(season) %>%
  nest(data_picks = c(pick_hash, path)) %>%
  ungroup()

# Define scoring function
score_picks_bl_1 <- function(data_scoring) {
  data_scoring %>%
    ungroup() %>%
    arrange(week) %>%
    mutate(
      is_correct = (result %in% c("W", "T")),
      count_deaths = cumsum(!is_correct),
      is_alive = count_deaths == 0,
      is_alive = lag(is_alive, 1)
    ) %>%
    replace_na(list(is_alive = TRUE)) %>%
    pull(is_alive) %>%
    sum() - 1
}

# Sample an "agent" pick
df_sim_bl_1 <- df_sim_results %>%
  left_join(grid_pool_picks, by = "season") %>%
  left_join(grid_results, by = "season") %>%
  mutate(picks_agent = map(data_picks, slice_sample, n = 1)) %>%
  unnest(picks_agent) %>%
  select(-data_picks)

# Determine performance
df_sim_bl_1 <- df_sim_bl_1 %>%
  mutate(
    data_agent_picks = map(path, read_parquet),
    data_scoring = map2(data_agent_picks, data_results, ~ left_join(.x, .y, by = c("week", "pick" = "team"))),
    agent_correct = map_dbl(data_scoring, score_picks_bl_1),
    return = case_when(
      agent_correct > top_correct ~ as.double(params$n_pool_players),
      agent_correct == top_correct ~ as.double(params$n_pool_players / (top_n + 1)),
      TRUE ~ as.double(0.0)
    )
  ) %>%
  select(-starts_with("data_"))

# Capture mean return
strike_pct_bl_1 <- df_sim_bl_1 %>%  mutate(strike = return > 0) %>% pull(strike) %>% mean()
return_mean_bl_1 <- df_sim_bl_1 %>% pull(return) %>% mean()
  

# BASELINE - BEST EXPECTED VALUE ------------------------------------------

# Load expected value data
df_ev <- read_parquet(here("data", "prepared", "df_ev.parquet"))

# Compile "agent" picks each sseason using best expected value
df_picks_ev_top1 <- 
  df_results_all %>%
  left_join(df_ev, by = c("season", "week", "team")) %>%
  group_by(season, week) %>%
  slice_max(order_by = expected_value, n = 1) %>%
  ungroup()

# Extract stats for each season
df_stats_ev_top1 <- df_picks_ev_top1 %>%
  group_by(season) %>%
  arrange(week) %>%
  mutate(
    is_correct = (result %in% c("W", "T")),
    count_deaths = cumsum(!is_correct),
    is_alive = count_deaths == 0,
    is_alive = lag(is_alive, 1)
  ) %>%
  replace_na(list(is_alive = TRUE)) %>%
  summarize(
    agent_correct = sum(is_alive) - 1
  ) %>%
  ungroup() %>%
  arrange(season)
  

# Determine performance
df_sim_bl_2 <- df_sim_results %>%
  left_join(df_stats_ev_top1, by = "season") %>%
  mutate(
    return = case_when(
      agent_correct > top_correct ~ as.double(params$n_pool_players),
      agent_correct == top_correct ~ as.double(params$n_pool_players / (top_n + 1)),
      TRUE ~ as.double(0.0)
    )
  )

# Capture mean return
strike_pct_bl_2 <- df_sim_bl_2 %>%  mutate(strike = return > 0) %>% pull(strike) %>% mean()
return_mean_bl_2 <- df_sim_bl_2 %>% pull(return) %>% mean()

# NOTES:  Looking at raw returns in df_sim_bl_2, it is clear that this strategy
# was VERY successful in 2015, and was able to win in every single simulation
# due to a high number of upsets early in that season. However, removing 2015
# from the evaluation dataset, this strategy becomes far less effective.

# BASELINE - TOP 3 EXPECTED VALUE -----------------------------------------

# Note:  Similar to above, but randomized to select one of the top 3 ranked
# teams each week via expected value. The purpose is to add some variation to
# the baseline strategy in case on eof the "best" teams lost early in a season.

# Load expected value data
df_ev <- read_parquet(here("data", "prepared", "df_ev.parquet"))

# Compile "agent" picks each season using a top-3 expected value pick
df_picks_ev_top3 <- 
  df_results_all %>%
  left_join(df_ev, by = c("season", "week", "team")) %>%
  group_by(season, week) %>%
  slice_max(order_by = expected_value, n = 3) %>%
  ungroup() %>%
  select(season, week, result)

# Define function for creating pick samples
sample_picks_ev_top3 <- function(season, data_picks_ev_top3) {
  
  data_picks_ev_top3 %>%
    filter(season == !!season) %>%
    group_by(season, week) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    arrange(week) %>%
    mutate(
      is_correct = (result %in% c("W", "T")),
      count_deaths = cumsum(!is_correct),
      is_alive = count_deaths == 0,
      is_alive = lag(is_alive, 1)
    ) %>%
    replace_na(list(is_alive = TRUE)) %>%
    pull(is_alive) %>%
    sum() - 1
  
}

# Determine performance
df_sim_bl_3 <- df_sim_results %>%
  mutate(
    agent_correct = map(
      season, 
      sample_picks_ev_top3, 
      data_picks_ev_top3 = df_picks_ev_top3
    ),
    return = case_when(
      agent_correct > top_correct ~ as.double(params$n_pool_players),
      agent_correct == top_correct ~ as.double(params$n_pool_players / (top_n + 1)),
      TRUE ~ as.double(0.0)
    )
  )

# Capture mean return
strike_pct_bl_3 <- df_sim_bl_3 %>%  mutate(strike = return > 0) %>% pull(strike) %>% mean()
return_mean_bl_3 <- df_sim_bl_3 %>% pull(return) %>% mean()



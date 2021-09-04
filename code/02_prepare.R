# The purpose of this script is to prepare the data for analysis.
#   - Data cleaning
#   - Feature engineering

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")
source("./code/funs_utils.R")
source("./code/funs_prepare.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))

# Load lookup table
df_teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# Setup parallel processing
plan(multisession, workers = params$n_cores)

# CLEAN DATA - ELO ------------------------------------------------------------

# Load raw ELO data
df_elo_latest <- read_parquet(here("data", "raw", "df_elo_latest.parquet"))
df_elo_hist <- read_parquet(here("data", "raw", "df_elo_hist.parquet"))

# Bind datasets
df_elo_raw <- bind_rows(df_elo_hist, df_elo_latest)
rm(df_elo_latest, df_elo_hist)

# Filter games
df_elo <- df_elo_raw %>% 
  filter(
    season >= params$year_min,  # min year
    ! season %in% params$years_exclude,
    is.na(playoff)  # remove playoff weeks
  )

# Remove duplicates (possible if between seasons and current season is also in historic data)
df_elo <- df_elo %>% distinct()

# Calculate week for each game
df_elo <- df_elo %>%
  mutate(
    season = as.integer(season),
    date_rounded = floor_date(date, unit = "week", week_start = 3)
  ) %>%
  group_by(season) %>%
  mutate(
    week = dense_rank(date_rounded)
  ) %>%
  ungroup()

# Clean team names
df_elo <- df_elo %>%
  left_join(
    df_teams %>% select(team1 = team_short, team1_master = team_master_short),
    by = "team1"
  ) %>%
  left_join(
    df_teams %>% select(team2 = team_short, team2_master = team_master_short),
    by = "team2"
  ) %>%
  mutate(
    team1_master = ifelse(is.na(team1_master), team1, team1_master),
    team2_master = ifelse(is.na(team2_master), team2, team2_master)
  ) %>%
  select(-team1, -team2) %>%
  rename(
    team_home = team1_master, 
    team_away = team2_master,
    elo_home = elo1_pre,
    elo_away = elo2_pre,
    prob_home = elo_prob1
  )

# Calculate complementary probability for team 2
df_elo <- df_elo %>%
  mutate(prob_away = 1 - prob_home)

# Reorganize columns
df_elo_out <- df_elo %>%
  select(
    season,
    week,
    date,
    team_home,
    team_away,
    elo_home,
    elo_away,
    prob_home,
    prob_away
  )

# Save data
write_parquet(df_elo_out, here("data", "prepared", "df_elo.parquet"))


# EXTRACT RESULTS DATA ----------------------------------------------------

# Extract result data
df_results <- df_elo %>%
  select(season, week, date, team_home, team_away, score_home = score1, score_away = score2) %>%
  mutate(
    winner_location = case_when(
      score_home > score_away ~ "home",
      score_away > score_home ~ "away",
      score_home == score_away ~ "tie"
    )
  ) %>%
  pivot_longer(
    cols = c(team_home, team_away),
    names_to = "location",
    names_prefix = "team_",
    values_to = "team"
  ) %>%
  mutate(
    result = case_when(
      winner_location == location ~ "W",
      winner_location == "tie" ~ "T",
      TRUE ~ "L"
    )
  ) %>%
  select(season, week, team, result)

# Write to disk
write_parquet(df_results, here("data", "prepared", "df_results.parquet"))


# CLEAN DATA - PICK DISTRIBUTIONS ---------------------------------------------

# Load data
df_pick_dist <- read_parquet(here("data", "raw", "df_pick_dist.parquet"))

# Clean data
df_pick_dist <- df_pick_dist %>%
  mutate(
    team = str_extract(team, "[:upper:]{2,3}"),
    pick_pct = str_remove_all(pick_pct, "\\%") %>% as.numeric() / 100
  ) %>%
  replace_na(list(pick_pct = 0)) %>%
  left_join(
    df_teams %>% select(team = team_short, team_master = team_master_short),
    by = "team"
  ) %>%
  mutate(team = team_master) %>%
  select(-team_master) %>%
  select(season, week, team, pick_pct)

# Save data
write_parquet(df_pick_dist, here("data", "prepared", "df_pick_dist.parquet"))


# COMPILE BASELINE TEAM STATUS --------------------------------------------

# Create reference lookup (filtered for single week)
df_team_stats_weekly <- 
  df_elo %>%
  arrange(date) %>%
  group_by(season, week) %>%
  arrange(team_home, team_away) %>%
  mutate(game_id = 1:n()) %>%
  ungroup() %>%
  arrange(season, week, game_id) %>%
  select(
    season,
    week, 
    game_id, 
    team_home,
    team_away,
    elo_home,
    elo_away,
    prob_home,
    prob_away
  )

# Reshape longer (one record/row per team per week)
df_team_stats_weekly <- df_team_stats_weekly %>%
  pivot_longer(
    cols = c(team_home, team_away),
    names_to = "location",
    names_prefix = "team_",
    values_to = "team"
  ) %>%
  mutate(
    elo = case_when(
      location == "home" ~ elo_home,
      location == "away" ~ elo_away
    ),
    prob = case_when(
      location == "home" ~ prob_home,
      location == "away" ~ prob_away
    )
  ) %>%
  select(
    season,
    week,
    game_id,
    team,
    location,
    elo,
    prob
  )

# Join pick distributions
df_team_stats_weekly <- df_team_stats_weekly %>%
  left_join(df_pick_dist, by = c("season", "week", "team"))

# Impute zero if blank (sometimes survivor grid has NA when no picks exist)
df_team_stats_weekly <- df_team_stats_weekly %>%
  replace_na(list(pick_pct = 0))


# CALCULATE EXPECTED VALUE ----------------------------------------------------

# Cleanup workspace
rm(df_elo, df_pick_dist, df_matchups)
gc()

# Calculate expected value
grid_ev <- df_team_stats_weekly %>%
  select(season, week) %>%
  distinct() %>%
  mutate(
    run = future_map2(
      season, week, 
      safely(calculate_expected_value, otherwise = NA), 
      data_metrics = df_team_stats_weekly
    ),
    data_ev = map(run, "result"),
    error = map(run, "error"),
    .progress = TRUE
  )

# Print grid
print(grid_ev, n = nrow(grid_ev))

# Unnest grid
df_ev <- grid_ev %>%
  select(-run, -error) %>%
  unnest(data_ev)

# Write to disk
write_parquet(df_ev, here("data", "prepared", "df_ev.parquet"))

# Add expected value to weekly team stats
df_team_stats_weekly <- df_team_stats_weekly %>%
  left_join(df_ev, by = c("season", "week", "team"))


# COMBINE FINAL WEEKLY FEATURES ----------------------------------------------

# Pivot wider (one record per week, team stats in feature columns)
df_features <- df_team_stats_weekly %>%
  mutate(team = str_to_lower(team)) %>%
  pivot_wider(
    id_cols = c(season, week),
    names_from = team, 
    values_from = c(location, elo, prob, pick_pct, expected_value),
    values_fill = list(location = "bye", prob = 0, pick_pct = 0, expected_value = 0)
  ) %>%
  fill(starts_with("elo_"), .direction = "down")

# Write to disk
write_parquet(df_features, here("data", "prepared", "df_features.parquet"))

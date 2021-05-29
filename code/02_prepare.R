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


# CLEAN DATA - ELO ------------------------------------------------------------

# Load raw ELO data
df_elo_latest <- read_parquet(here("data", "raw", "df_elo_latest.parquet"))
df_elo_hist <- read_parquet(here("data", "raw", "df_elo_hist.parquet"))

# Bind datasets
df_elo <- bind_rows(df_elo_hist, df_elo_latest)
rm(df_elo_latest, df_elo_hist)

# Filter for min year
df_elo <- df_elo %>% filter(season >= params$year_min)

# Calculate week for each game
df_elo <- df_elo %>%
  mutate(
    season = as.integer(season),
    date_rounded = round_date(date, unit = "week")
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
df_elo <- df_elo %>%
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
write_parquet(df_elo, here("data", "prepared", "df_elo.parquet"))


# CLEAN DATA - PICK DISTRIBUTIONS ---------------------------------------------

# Load data
df_pick_dist <- read_parquet(here("data", "raw", "df_pick_dist.parquet"))

# Clean data
df_pick_dist <- df_pick_dist %>%
  mutate(
    team = str_extract(team, "[:upper:]{2,3}"),
    pick_pct = str_remove_all(pick_pct, "\\%") %>% as.numeric() / 100
  ) %>%
  left_join(
    df_teams %>% select(team = team_short, team_master = team_master_short),
    by = "team"
  ) %>%
  mutate(team = team_master) %>%
  select(-team_master) %>%
  select(season, week, team, pick_pct)

# Save data
write_parquet(df_pick_dist, here("data", "prepared", "df_pick_dist.parquet"))


# COMPILE MATCHUP DATA ----------------------------------------------------

# Extract matchup data
df_matchups <- df_elo %>%
  select(season, week, date, team_home, team_away)

# Write to disk
write_parquet(df_matchups, here("data", "prepared", "df_matchups.parquet"))

# CALCULATE EXPECTED VALUE ----------------------------------------------------

# Create reference lookup (filtered for single week)
df_metrics <- 
  df_elo %>%
  arrange(date) %>%
  group_by(season, week) %>%
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
df_metrics <- df_metrics %>%
  pivot_longer(
    cols = c(team_home, team_away),
    names_to = "location",
    values_to = "team"
  ) %>%
  mutate(
    location = str_remove(location, "^team_"),
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


# GENERATE ALL SCENARIOS BY WEEK ----------------------------------------------

grid_expval <- df_metrics %>%
  select(season, week) %>%
  distinct() %>%
  mutate(
    data_expval = map2(season, week, calculate_expected_value, data_metrics = df_metrics)
  )

df_expval <- grid_expval %>%
  unnest(data_expval)


# SAVE DATA -------------------------------------------------------------------

write_rds(df.expval, here("data", "output", "expected_value.rds"))
write_csv(df.expval, here("data", "output", "expected_value.csv"))
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

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))


# CLEAN DATA - ELO ------------------------------------------------------------

# Load raw ELO data
df.elo.current <- read_rds(here("data", "raw", "data_elo_current.rds"))
df.elo.historic <- read_rds(here("data", "raw", "data_elo_historic.rds"))

# Bind datasets
df.elo <- bind_rows(df.elo.historic, df.elo.current)
rm(df.elo.current, df.elo.historic)

# Calculate week for each game
df.elo <- df.elo %>%
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
df.elo <- df.elo %>%
  left_join(
    lu.teams %>% select(team1 = team_short, team1_master = team_master_short),
    by = "team1"
  ) %>%
  left_join(
    lu.teams %>% select(team2 = team_short, team2_master = team_master_short),
    by = "team2"
  ) %>%
  mutate(
    team1_master = ifelse(is.na(team1_master), team1, team1_master),
    team2_master = ifelse(is.na(team2_master), team2, team2_master)
  ) %>%
  select(-team1, -team2) %>%
  rename(
    team1 = team1_master, 
    team2 = team2_master,
    elo_team1 = elo1,
    elo_team2 = elo2,
    prob_team1 = elo_prob1
  )

# Calculate complementary probability for team 2
df.elo <- df.elo %>%
  mutate(prob_team2 = 1 - prob_team1)

# Reorganize columns
df.elo <- df.elo %>%
  select(
    season,
    week,
    date,
    team1,
    team2,
    elo_team1,
    elo_team2,
    prob_team1,
    prob_team2
  )

# Save data
write_rds(df.elo, here("data", "prepared", "data_elo.rds"))


# CLEAN DATA - PICK DISTRIBUTIONS ---------------------------------------------

# Load/Clean data
df.dist <- 
  read_rds(here("data", "raw", "data_pickdist.rds")) %>%
  clean_pickdist()

# Save data
write_rds(df.dist, here("data", "prepared", "data_pickdist.rds"))


# CALCULATE EXPECTED VALUE ----------------------------------------------------

# Create reference lookup (filtered for single week)
df.metrics <- df.elo %>%
  arrange(date) %>%
  group_by(season, week) %>%
  mutate(game_id = 1:n()) %>%
  ungroup() %>%
  arrange(season, week, game_id) %>%
  select(season, week, game_id, team1, team2, prob_team1, prob_team2)


# Reshape longer, join pick percentage data
df.metrics <- 
  bind_rows(
    df.metrics %>% mutate(team_id = "team1") %>% select(season, week, game_id, team_id, team = team1, win_prob = prob_team1),
    df.metrics %>% mutate(team_id = "team2") %>% select(season, week, game_id, team_id, team = team2, win_prob = prob_team2)
  ) %>%
  left_join(
    df.dist %>% select(season, week, team, pick_pct),
    by = c("season", "week", "team")
  )

# Drop NA values
df.metrics <- df.metrics %>% drop_na()


# GENERATE ALL SCENARIOS BY WEEK ----------------------------------------------

grid.expval <- df.metrics %>%
  select(season, week) %>%
  distinct() %>%
  mutate(data_expval = map2(season, week, calculate_expected_value, data_metrics = df.metrics))

df.expval <- grid.expval %>%
  unnest(data_expval)


# SAVE DATA -------------------------------------------------------------------

write_rds(df.expval, here("data", "output", "expected_value.rds"))
write_csv(df.expval, here("data", "output", "expected_value.csv"))
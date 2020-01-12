# The purpose of this script is to prepare the data for analysis.
#   - Data cleaning
#   - Feature engineering

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(here)
library(readr)

# Source functions
source(here("code", "funs_prepare.R"))

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))


# CLEAN DATA - ELO ------------------------------------------------------------

# Load/Clean the current season's data
df.elo.current <- 
  read_rds(here("data", "raw", "data_elo_current.rds")) %>%
  clean_elo()

# Load/Clean historical data
df.elo.historic <- 
  read_rds(here("data", "raw", "data_elo_historic.rds")) %>%
  clean_elo()

# Bind datasets together
df.elo <- 
  bind_rows(
    df.elo.historic, 
    df.elo.current
  ) %>%
  arrange(season, week)

# Remove temporary tables
rm(df.elo.current, df.elo.historic)

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
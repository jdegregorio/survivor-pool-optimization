# The purpose of this script is to scrape the pick dstribution data from
# survivorgrid.com. This website contains historical pick distribution data for
# survivor pools.

# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(here)
library(feather)
library(readr)

# Source functions
source(here("code", "funs_metrics.R"))

# Load data
df.elo <- read_feather(here("data", "output", "elo_prob.feather"))
df.dist <- read_feather(here("data", "output", "pickdist.feather"))


# PREPARE PICK DATA -----------------------------------------------------------

# Create reference lookup (filtered for single week)
df.metrics <- df.elo %>%
  arrange(date) %>%
  group_by(week) %>%
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

write_feather(df.expval, here("data", "output", "expected_value.feather"))
write_csv(df.expval, here("data", "output", "expected_value.csv"))

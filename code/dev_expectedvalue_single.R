# The purpose of this script is to scrape the pick dstribution data from
# survivorgrid.com. This website contains historical pick distribution data for
# survivor pools.

# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(here)
library(feather)
library(readr)

# Load data
df.elo <- read_feather(here("data", "output", "elo_prob.feather"))
df.dist <- read_feather(here("data", "output", "pickdist.feather"))

# Define parameters (target week)
ftr.season <- 2019
ftr.week <- 6

# DEV - CALCULATE EXPECTED VALUE (ONE WEEK) -----------------------------------

# Filter pick distribution data for single example week
tmp.dist <- df.dist %>% filter(season == ftr.season, week == ftr.week)

# Create reference lookup (filtered for single week)
lu.ref <- df.elo %>% 
  filter(season == ftr.season, week == ftr.week) %>%
  mutate(game_id = 1:n()) %>%
  select(game_id, team1, team2, prob_team1, prob_team2)

# Reshape longer
lu.ref <- bind_rows(
  lu.ref %>% mutate(team_id = "team1") %>% select(game_id, team_id, team = team1, win_prob = prob_team1),
  lu.ref %>% mutate(team_id = "team2") %>% select(game_id, team_id, team = team2, win_prob = prob_team2)
)

# Create grid of possible game outcomes
df.outcomes <- 
  gtools::permutations(2, length(unique(lu.ref$game_id)), v = c("team1", "team2"), repeats.allowed = TRUE) %>%
  as_tibble() %>%
  mutate(scenario_id = 1:n()) %>%
  pivot_longer(-scenario_id, names_to = "game_id", values_to = "winner") %>%
  mutate(game_id = str_sub(game_id, 2) %>% as.integer()) %>%
  left_join(
    lu.ref %>% select(game_id, team_id, team, win_prob), 
    by = c("game_id" = "game_id", "winner" = "team_id")
  ) %>%
  left_join(
    tmp.dist %>% select(team, pick_pct), 
    by = "team"
  )

# Determine key scenario metrics (probability, value)

df.winners <- df.outcomes %>%
  select(scenario_id, game_id, team) %>%
  pivot_wider(id_cols = scenario_id, names_from = game_id, values_from = team)

df.probs <- df.outcomes %>%
  select(scenario_id, game_id, win_prob) %>%
  pivot_wider(id_cols = scenario_id, names_from = game_id, values_from = win_prob) %>%
  mutate(prob = matrixStats::rowProds(as.matrix(.[,-1]))) %>%
  select(scenario_id, prob)

df.value <- df.outcomes %>%
  select(scenario_id, game_id, pick_pct) %>%
  pivot_wider(id_cols = scenario_id, names_from = game_id, values_from = pick_pct) %>%
  mutate(value = 1 / rowSums(as.matrix(.[,-1]))) %>%
  select(scenario_id, value)

df.metrics <- df.winners %>%
  pivot_longer(-scenario_id, names_to = "game_id", values_to = "team") %>%
  inner_join(df.probs, by = "scenario_id") %>%
  inner_join(df.value, by = "scenario_id") %>%
  mutate(term = prob * value)


# Summarize expected value by team
df.expval <- df.metrics %>%
  group_by(team) %>%
  summarize(expected_value = sum(term))

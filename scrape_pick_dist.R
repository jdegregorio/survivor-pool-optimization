# The purpose of this script is to scrape the pick dstribution data from
# survivorgrid.com. This website contains historical pick distribution data for
# survivor pools.

# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(here)
library(feather)
library(readr)

# Source functions
source(here("R", "scrape_survgrid.R"))

# Define parameters
season.start <- 2017
season.current <- if_else(month(Sys.Date()) >= 8, year(Sys.Date()), year(Sys.Date()) - 1)

# IDENTIFY MISSING DATA -------------------------------------------------------

# Load the latest game result saved data
df.dist.existing <- read_feather(here("output", "pickdist.feather"))

# Summarize available data
seasons.available <- df.dist.existing$season %>% unique()
seasons.available <- seasons.available[!seasons.available == season.current]

# Summarize required seasons
seasons.required <- season.start:season.current
seasons.required <- seasons.required[!seasons.required %in% seasons.available]


# SCRAPE RAW DATA -------------------------------------------------------------

grid.dist <-
  crossing(
    season = seasons.required,
    week = 1:2
  ) %>%
  mutate(
    url = generate_url_survgrid(season, week),
    results = map(url, scrape_survgrid, delay = 3)
  )


# CLEAN RESULTS ---------------------------------------------------------------

df.dist.new <- 
  grid.dist %>%
  unnest(results) %>%
  mutate(
    team = str_extract(team, "[:upper:]{2,3}"),
    pick_pct = str_remove_all(pick_pct, "\\%") %>% as.numeric() / 100,
    win_prob = str_remove_all(win_prob, "\\%") %>% as.numeric() / 100,
    expected_value = as.numeric(expected_value)
  ) %>%
  select(season, week, team, pick_pct, win_prob, expected_value)



# JOIN NEW/EXISTING GAME DATA -------------------------------------------------

# Bind dataframes
df.dist <- 
  bind_rows(
    df.dist.existing %>% mutate(priority = 2), 
    df.dist.new %>% mutate(priority = 1)
  )


# Remote duplicates, take most recent
df.dist <- df.dist %>%
  group_by(season, week, team) %>%
  arrange(priority) %>%
  slice(1) %>%
  ungroup() %>%
  select(-priority)


# SAVE DATA -------------------------------------------------------------------

write_feather(df.dist, here("output", "pickdist.feather"))
write_csv(df.dist, here("output", "pickdist.csv"))

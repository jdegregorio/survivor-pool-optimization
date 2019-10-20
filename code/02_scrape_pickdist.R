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
source(here("code", "funs_scrape_pickdist.R"))

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# Define parameters
# season.start <- 2010
season.current <- if_else(month(Sys.Date()) >= 8, year(Sys.Date()), year(Sys.Date()) - 1)
season.start <- season.current

# IDENTIFY MISSING DATA -------------------------------------------------------

# Load the latest game result saved data
df.dist.existing <- read_feather(here("data", "output", "pickdist.feather"))

df.available <- df.dist.existing %>%
  select(season, week) %>%
  distinct()


# SCRAPE RAW DATA -------------------------------------------------------------

# Scrape data
grid.dist <-
  crossing(
    season = season.start:season.current,
    week = 1:17
  ) %>%
  anti_join(df.available, by = c("season", "week")) %>%
  mutate(
    url = generate_url_survgrid(season, week),
    results = map(url, possibly(scrape_survgrid, otherwise = NA), delay = 3)
  )

# Filter for sucessfully scraped pages
grid.dist <- grid.dist %>%
  mutate(class = map_chr(results, class)) %>%
  filter(class == "data.frame")

# End script if no new data is available
if (nrow(grid.dist) == 0) {stop("No new data is available")}


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
  left_join(
    lu.teams %>% select(team = team_short, team_master = team_master_short), 
    by = "team"
  ) %>%
  mutate(team = team_master) %>%
  select(-team_master) %>%
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

write_feather(df.dist, here("data", "output", "pickdist.feather"))
write_csv(df.dist, here("data", "output", "pickdist.csv"))

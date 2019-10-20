# The purpose of this script is to scrape the nfl game results from
# pro-football-reference.com

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
source(here("code", "funs_scrape_pfr.R"))

# Define parameters
season.start <- 2000
season.current <- if_else(month(Sys.Date()) >= 8, year(Sys.Date()), year(Sys.Date()) - 1)

# IDENTIFY MISSING DATA -------------------------------------------------------

# Load the latest game result saved data
df.games.existing <- read_feather(here("data", "output", "games.feather"))

# Summarize available data
seasons.available <- df.games.existing$season %>% unique()
seasons.available <- seasons.available[!seasons.available == season.current]

# Summarize required seasons
seasons.required <- season.start:season.current
seasons.required <- seasons.required[!seasons.required %in% seasons.available]


# SCRAPE RAW DATA -------------------------------------------------------------

grid.games <-
  tibble(season = seasons.required) %>%
  mutate(
    url = generate_url_pfr(season),
    results = map(url, scrape_pfr, delay = 3)
  )

# CLEAN RESULTS ---------------------------------------------------------------

df.games.new <- 
  grid.games %>%
  unnest(results) %>%
  mutate_at(vars(matches("^points|^yards|^turnovers")), as.numeric) %>%
  filter(link_boxscore %in% c("boxscore", "preview")) %>%
  mutate(
    year = if_else(str_detect(date, "^Sep|^Oct|^Nov|^Dec"), as.numeric(season), as.numeric(season + 1)),
    datetime = ymd_hm(paste(year, date, time)),
    date = date(datetime),
    weekday = wday(datetime, label = TRUE),
    hour = hour(datetime),
    week = str_trim(week),
    week = case_when(
      week == "WildCard" ~ as.numeric(18),
      week == "Division" ~ as.numeric(19),
      week == "ConfChamp" ~ as.numeric(20),
      week == "SuperBowl" ~ as.numeric(21),
      TRUE ~ as.numeric(week)
    ),
    flag_playoffs = week > 17,
    flag_neutral = (at_indicator == "N"),
    team1 = winner,
    team2 = loser,
    location_team1 = case_when(
      at_indicator == "@" ~ "away",
      at_indicator == "" ~ "home",
      at_indicator == "N" ~ "neutral"
    ),
    location_team2 = case_when(
      at_indicator == "@" ~ "home",
      at_indicator == "" ~ "away",
      at_indicator == "N" ~ "neutral"
    ),
    points_team1 = points_winner,
    points_team2 = points_loser
  ) %>%
  group_by(season, week) %>%
  arrange(date, hour) %>%
  mutate(game = 1:n()) %>%
  ungroup() %>%
  select(
    season, 
    week, 
    game,
    date, 
    weekday, 
    hour, 
    team1, 
    team2, 
    points_team1, 
    points_team2, 
    location_team1, 
    location_team2, 
    flag_playoffs, 
    flag_neutral
  )


# JOIN NEW/EXISTING GAME DATA -------------------------------------------------

# Bind dataframes
df.games <- 
  bind_rows(
    df.games.existing %>% mutate(priority = 2), 
    df.games.new %>% mutate(priority = 1)
  )


# Remote duplicates, take most recent
df.games <- df.games %>%
  group_by(season, week, game) %>%
  arrange(priority) %>%
  slice(1) %>%
  ungroup() %>%
  select(-priority)



# SAVE DATA -------------------------------------------------------------------

write_feather(df.games, here("data", "output", "games.feather"))
write_csv(df.games, here("data", "output", "games.csv"))

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

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# Define parameters
season.current <- if_else(month(Sys.Date()) >= 8, year(Sys.Date()), year(Sys.Date()) - 1)

# SCRAPE RAW DATA -------------------------------------------------------------

url <- generate_url_pfr(season.current)
df.schedule <- scrape_pfr(url)

# CLEAN DATA ------------------------------------------------------------------

df.schedule <- df.schedule %>%
  mutate_at(vars(matches("^points|^yards|^turnovers")), as.numeric) %>%
  filter(link_boxscore %in% c("boxscore", "preview")) %>%
  mutate(
    season = season.current,
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
  left_join(
    lu.teams %>% select(team1 = team_full, team1_master = team_master_short), 
    by = "team1"
  ) %>%
  left_join(
    lu.teams %>% select(team2 = team_full, team2_master = team_master_short), 
    by = "team2"
  ) %>%
  mutate(team1 = team1_master, team2 = team2_master) %>%
  select(-team1_master, -team2_master) %>%
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


# SAVE DATA -------------------------------------------------------------------

write_rds(df.schedule, here("data", "output", "schedule.rds"))
write_csv(df.schedule, here("data", "output", "schedule.csv"))

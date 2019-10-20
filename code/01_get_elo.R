# The purpose of this script is to call an API to five-thirty-eight to gather
# the latest win probabilities for the current season.

# https://projects.fivethirtyeight.com/nfl-api/2019/nfl_games_2019.csv 


# SETUP -----------------------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(feather)
library(readr)
library(httr)

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# Define Parameters
season.current <- if_else(month(Sys.Date()) >= 8, year(Sys.Date()), year(Sys.Date()) - 1)

# REQUEST DATA ----------------------------------------------------------------

# Define user agent
ua_text <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
ua <- user_agent(ua_text)
set_config(ua)

# Define URL
url <- paste0("https://projects.fivethirtyeight.com/nfl-api/", season.current, "/nfl_games_", season.current, ".csv")

# Make GET request
response <- GET(url, ua, timeout(60))

# Extract to dataframe
df.elo <- 
  content(response, type = "text", encoding = "UTF-8") %>%
  read_csv() %>%
  mutate(season = as.integer(season)) %>%
  left_join(
    lu.teams %>% select(team1 = team_short, team1_master = team_master_short), 
    by = "team1"
  ) %>%
  left_join(
    lu.teams %>% select(team2 = team_short, team2_master = team_master_short), 
    by = "team2"
  ) %>%
  mutate(team1 = team1_master, team2 = team2_master) %>%
  select(-team1_master, -team2_master) %>%
  select(
    season,
    date,
    team1,
    team2,
    elo_team1 = elo1,
    elo_team2 = elo2,
    prob_team1 = elo_prob1
  ) %>%
  mutate(prob_team2 = 1 - prob_team1)


# SAVE DATA -------------------------------------------------------------------

write_feather(df.elo, here("data", "output", "elo_prob.feather"))
write_csv(df.elo, here("data", "output", "elo_prob.csv"))

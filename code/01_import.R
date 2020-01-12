# The purpose of this script is to import the needed raw data for the analysis.

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")
source("./code/funs_utils.R")
source("./code/funs_import.R")

# Parameters
season.current <- get_current_season()

# Define user agent
ua_text <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
ua <- user_agent(ua_text)
set_config(ua)

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# IMPORT HISTORIC ELO DATA (FIVE-THIRTY-EIGHT) --------------------------------

# Define URL
url <- "https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv"

# Make GET request
response <- GET(url, ua, timeout(60))

# Extract to dataframe
df.elo.historic <- 
  content(response, type = "text", encoding = "UTF-8") %>%
  read_csv()

# Save raw data
write_rds(df.elo.historic, here("data", "raw", "data_elo_historic.rds"))


# IMPORT CURRENT ELO DATA (FIVE-THIRTY-EIGHT) ---------------------------------

# Define URL
url <- paste0("https://projects.fivethirtyeight.com/nfl-api/", season.current, "/nfl_games_", season.current, ".csv")

# Make GET request
response <- GET(url, ua, timeout(60))

# Extract to dataframe
df.elo.current <- 
  content(response, type = "text", encoding = "UTF-8") %>%
  read_csv() 

# Save raw data
write_rds(df.elo.current, here("data", "raw", "data_elo_current.rds"))


# IMPORT PICK DISTRIBUTION DATA -----------------------------------------------

#  Specify seasons/weeks
df.dist <- crossing(
  season = 2010:season.current,
  week = 1:17
)

# Scrape data
df.dist <- df.dist %>%
  mutate(
    data = map2(
      season, week, 
      possibly(scrape_pickdist, otherwise = NA), 
      delay = 3
    )
  ) %>%
  filter(!is.na(data)) %>%
  unnest(data)

# Save data
write_rds(df.dist, here("data", "raw", "data_pickdist.rds"))
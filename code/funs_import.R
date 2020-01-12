# The purpose of the following functions is import information for analysis.

# https://projects.fivethirtyeight.com/nfl-api/2019/nfl_games_2019.csv
# https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv

# SETUP -----------------------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(readr)
library(httr)

# Define user agent
ua_text <- 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/35.0.1916.47 Safari/537.36'
ua <- user_agent(ua_text)
set_config(ua)

# Load lookup table
lu.teams <- read_csv(here("data", "lookup", "team_lookup.csv"))


# FUNCTIONS - GENERAL ---------------------------------------------------------

get_current_season <- function () {
  if_else(
    month(Sys.Date()) >= 8,  # check month (i.e. season starts in September)
    year(Sys.Date()),        # use current year
    year(Sys.Date()) - 1     # use previous year
  )
}

# FUNCTIONS - ELO DATA --------------------------------------------------------

#' Get historic ELO data
#'
#' @return dataframe, containing raw historic elo data
#' @export
#'
get_elo_historic <- function () {
  
  # Define URL
  url <- "https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv"
  
  # Make GET request
  response <- GET(url, ua, timeout(60))
  
  # Extract to dataframe
  df.elo.historic <- 
    content(response, type = "text", encoding = "UTF-8") %>%
    read_csv()
  
  return(df.elo.historic)
}

#' Get ELO Data for Current Season
#'
#' @return dataframe, containing raw elo data
#' @export
#'
get_elo_current <- function () {
  
  # Determine current season
  season.current <- get_current_season()
  
  # Define URL
  url <- paste0("https://projects.fivethirtyeight.com/nfl-api/", season.current, "/nfl_games_", season.current, ".csv")
  
  # Make GET request
  response <- GET(url, ua, timeout(60))
  
  # Extract to dataframe
  df.elo.current <- 
    content(response, type = "text", encoding = "UTF-8") %>%
    read_csv() 
  
  return(df.elo.current)
}


# FUNCTIONS - PICK DISTRIBUTION DATA ------------------------------------------

#' Load Existing Pick Distribution Data
#'
#' Load the existing pick distribution data (if it exists).  If not, then load a
#' blank dataframe.
#'
#' @param path character, path to existing data
#'
#' @return dataframe, data containing existing data
#' @export
#'
load_existing_pickdist <- function(path) {
  if(file.exists(path)){
    data <- read_rds(path)
  } else {
    data <- tibble(
      season = integer(),
      week = integer(),
      team = character(),
      pick_pct = character()
    )
  }
  
  return(data)
}

#' Find Missing Pick Distribution Data (i.e. Week/Season records)
#'
#' @param data_existing dataframe, existing pick distribution data
#' @param season_start  integer, starting season
#' @param season_end integer, ending season
#'
#' @return dataframe, containing season/week pairs of missing records
#' @export
#'
find_missing_pickdist <- function (data_existing,
                                   season_start = 2010,
                                   season_end = get_current_season()) {
  
  # Compile complete list of required seasons/weeks
  df.required <- crossing(
    season = season_start:season_end,
    week = 1:17
  )
  
  # Examine existing data
  df.existing <- 
    data_existing %>%
    select(season, week) %>%
    distinct()
  
  # Determine missing data
  df.missing <- anti_join(df.required, df.existing, by = c("season", "week"))
  
  # Return missing weeks
  return(df.missing)
}


#' Get Pick Distribution Data
#' 
#' The purpose of this function is to scrape the pick dstribution data from
#' survivorgrid.com. This website contains historical pick distribution data for
#' survivor pools.
#'
#' @param season integer, year for pulling game history
#' @param week integer, week for pulling game history
#' @param delay integer, seconds delay after requesting page
#'
#' @return dataframe, containing pick distribution data for selected season
#' @export
#'
scrape_pickdist <- function(season, week, delay = 3) {
  
  # Construct url
  url <- paste0("https://www.survivorgrid.com/", season, "/", week)
  
  # Load raw results
  data <-
    url %>%
    xml2::read_html(options = "RECOVER") %>%
    rvest::html_node("#grid") %>%
    rvest::html_table() %>%
    select(
      team = Team,
      pick_pct = `P%`
    ) %>%
    as_tibble()
  
  # Delay (optional)
  Sys.sleep(delay)
  
  return(data)
}

#' Import Pick Distribution Data
#'
#' Import pick distribution data for all of the missing required records.  Data
#' is scraped from www.survivorgrid.com.
#'
#' @param data_missing dataframe, containing season/week of records to scrape
#'
#' @return dataframe, containing pick distribution data for requested records
#' @export
#'
get_pickdist <- function (data_missing) {
  
  # Scrape data, if missing
  if (nrow(data_missing) > 0) {
    
    # Scrape data
    df.dist.missing <-
      data_missing %>%
      mutate(
        data = map2(
          season, week, 
          possibly(scrape_pickdist, otherwise = NA), 
          delay = 3
        )
      ) %>%
      filter(!is.na(data)) %>%
      unnest(data)
    
  } else {
    
    # Create empty dataframe
    df.dist.missing <- tibble(
      season = integer(),
      week = integer(),
      team = character(),
      pick_pct = character()
    )
    
  }
  
  return(df.dist.missing)
}


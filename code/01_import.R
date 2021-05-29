# The purpose of this script is to import the needed raw data for the analysis.

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")
source("./code/funs_utils.R")
source("./code/funs_import.R")

# Parameters
season_current <- get_current_season()


# IMPORT HISTORIC ELO DATA (FIVE-THIRTY-EIGHT) --------------------------------

# Download file
url <- "https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv"
download.file(url, here("data", "raw", "data_elo_historic.csv"))

# Parse delimited file and save as parquet
df_elo_hist <- read_csv(here("data", "raw", "data_elo_historic.csv"), guess_max = 25000)
write_parquet(df_elo_hist, here("data", "raw", "df_elo_hist.parquet"))
file.remove(here("data", "raw", "data_elo_historic.csv"))


# IMPORT CURRENT ELO DATA (FIVE-THIRTY-EIGHT) ---------------------------------

# Download file
url <- paste0("https://projects.fivethirtyeight.com/nfl-api/nfl_elo_latest.csv")
download.file(url, here("data", "raw", "data_elo_latest.csv"))

# Parse delimited file and save as parquet
df_elo_latest <- read_csv(here("data", "raw", "data_elo_latest.csv"), guess_max = 25000)
write_parquet(df_elo_latest, here("data", "raw", "df_elo_latest.parquet"))
file.remove(here("data", "raw", "data_elo_latest.csv"))


# IMPORT PICK DISTRIBUTION DATA -----------------------------------------------

#  Specify seasons/weeks
df_pick_dist <- crossing(
  season = 2010:season_current,
  week = 1:17
)

# Scrape data
df_pick_dist <- df_pick_dist %>%
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
write_parquet(df_pick_dist, here("data", "raw", "df_pick_dist.parquet"))
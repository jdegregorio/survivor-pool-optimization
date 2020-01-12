# The purpose of this script is to import the needed raw data for the analysis.

# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(here)

# Source functions
source(here("code", "funs_import.R"))

# Define parameters
path.pickdist <- here("data", "raw", "data_pickdist.rds")

# IMPORT ELO DATA (FIVE-THIRTY-EIGHT) -----------------------------------------

# Download raw data
df.elo.historic <- get_elo_historic()
df.elo.current <- get_elo_current()

# Save raw data
write_rds(df.elo.historic, here("data", "raw", "data_elo_historic.rds"))
write_rds(df.elo.current, here("data", "raw", "data_elo_current.rds"))


# IMPORT PICK DISTRIBUTION DATA -----------------------------------------------

# Load existing data (if available)
df.dist.existing <- load_existing_pickdist(path.pickdist)

# Determine missing records (i.e. not in existing dataset)
df.missing <- find_missing_pickdist(
  data_existing = df.dist.existing,
  season_start = 2010,
  season_end = get_current_season()
)

# Get the missing pick distribution data
df.dist.missing <- get_pickdist(df.missing)

# Merge existing/missing data
df.dist <- 
  bind_rows(df.dist.existing, df.dist.missing) %>%
  arrange(season, week)

# Save data
write_rds(df.dist, path.pickdist)


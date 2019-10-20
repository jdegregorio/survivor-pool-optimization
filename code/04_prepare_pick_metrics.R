# The purpose of this script is to prepare additional pick metric data that will
# be used to select picks.


# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(here)
library(feather)

# Source functions
# source(here("code", "funs_scrape_survgrid.R"))

# Load data
df.elo  <- read_feather(here("data", "output", "elo_prob.feather"))
df.dist <- read_feather(here("data", "output", "pickdist.feather"))




df.dist <- df.dist %>%
  select(season, week, team, pick_pct)

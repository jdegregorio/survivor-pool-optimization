# The purpose of this script is to scrape the pick dstribution data from
# survivorgrid.com. This website contains historical pick distribution data for
# survivor pools.

# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(purrr)
library(here)
library(feather)
library(readr)

# Source functions
source(here("code", "funs_metrics.R"))

# Load data
df.elo <- read_rds(here("data", "prepared", "data_elo.rds"))
df.dist <- read_rds(here("data", "prepared", "data_pickdist.rds"))



# The purpose of this script is to train a model to predict the pick
# distribution for future weeks.

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
df.elo <- read_feather(here("data", "output", "elo_prob.feather"))
df.dist <- read_feather(here("data", "output", "pickdist.feather"))
# The purpose of this script is to prepare the final datasets for
# training/evolution.

# - Fitness Function - pull relevant features, simulate week to week


# Alternatively, would a simple analysis to look at all possible outcomes of the
# season to find a maximum probability of survival be more reasonable?  This
# could also be extended to consider multiple picks.

# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))

# Setup parallel processing
plan(multisession, workers = 6)



# LOAD DATA ---------------------------------------------------------------

df_features <- read_parquet(here("data", "prepared", "df_features.parquet"))

# Gather collection of available pick samples
df_player_status <- read_parquet(here("data", "sims_pool_seasons", "player_status", "2012", "2ba2e719.parquet"))
df_teams_remaining <- read_parquet(here("data", "sims_pool_seasons", "teams_remaining", "2012", "2ba2e719.parquet"))



tmp <- df_teams_remaining %>%
  pivot_wider(
    id_cols = week,
    names_from = player_id,
    values_from = starts_with("is_available")
  )


df_player_status %>%
  pivot_wider(
    id_cols = week,
    names_from = player_id,
    names_prefix = "is_alive_",
    values_from = is_alive
  )







# df_ <- 
#   list.files(here("data", "sims_pool_seasons", "player_status"), recursive = TRUE, full.names = TRUE) %>%
#   enframe("idx", "path") %>%
#   mutate(
#     season = str_extract(path, "[:digit:]{4}(?=\\/[:alnum:]{8}.parquet)") %>% as.numeric(),
#     season_hash = str_extract(path, "[:alnum:]{8}(?=.parquet)")
#   ) %>%
#   select(season, season_hash, path) %>%
#   slice_sample(n = 1) %>%
#   pull(path) %>%
#   pluck(1) %>%
#   read_parquet()

# The purpose of this script is to generate sample simulation of picks for each
# season based on the historic pick distributions each week.  These picks will
# later be used for simulating individual survivor pools.

# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))


# LOAD DATA ---------------------------------------------------------------

df_pick_dist <- read_parquet(here("data", "prepared", "df_pick_dist.parquet"))


# SIMULATE PICKS ----------------------------------------------------------

simulate_pool_picks <- function(season, data_pick_dist) {
  
  # Prepare simulation data
  df_sim <-
    tibble(week = 1:17) %>%
    left_join(data_pick_dist, by = "week") %>%
    nest_by(week) %>%
    mutate(pick = NA_character_)
  
  # Simulate weekly picks
  for (week in 1:17) {
    df_sim[week, "pick"] <- 
      df_sim$data[[week]] %>%
      filter(! team %in% df_sim$pick) %>%
      slice_sample(n = 1, weight_by = pick_pct) %>%
      pull(team)
  }
  
  # Generate file name
  file_name <- df_sim %>%
    pull(pick) %>%
    str_flatten() %>%
    md5() %>%
    str_sub(1, 8) %>%
    str_c(".parquet")
  
  # Generate path/folder
  dir.create(here("data", "sims_pool_picks", season), recursive = T, showWarnings = FALSE)
  
  # Write to disk results
  df_sim <- df_sim %>% 
    select(week, pick) %>%
    write_parquet(here("data", "sims_pool_picks", season, file_name))
  
  return(NULL)
}


# Create 1000 simulated picks per season based on historic pick distributions
grid_sim <-
  df_pick_dist %>%
  nest_by(season) %>%
  filter(
    season >= params$year_min,
    ! season %in% params$years_exclude,
  ) %>%
  mutate(sim_id = list(1:params$n_pool_pick_sims)) %>%
  unnest(sim_id) %>%
  mutate(run = map2(season, data, simulate_pool_picks))

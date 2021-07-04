# The purpose of this module is to generate week-to-week simulations of an NFL
# season in order to act as the environment for agents to compete within.

library(tidyverse)
library(arrow)
library(here)

df_elo <- read_parquet(here("data", "prepared", "df_elo.parquet"))
df_results <- read_parquet(here("data", "prepared", "df_results.parquet"))

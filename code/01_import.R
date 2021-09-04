# The purpose of this script is to import the needed raw data for the analysis.

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

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


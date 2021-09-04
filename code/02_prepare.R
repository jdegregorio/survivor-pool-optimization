# The purpose of this script is to prepare the data for analysis.
#   - Data cleaning
#   - Feature engineering

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))

# Load lookup table
df_teams <- read_csv(here("data", "lookup", "team_lookup.csv"))

# Setup parallel processing
# plan(multisession, workers = params$n_cores)


# LOAD DATA ---------------------------------------------------------------

# Load raw ELO data
df_elo_latest <- read_parquet(here("data", "raw", "df_elo_latest.parquet"))
df_elo_hist <- read_parquet(here("data", "raw", "df_elo_hist.parquet"))


# BASIC DATA PREPARATION --------------------------------------------------

# Bind datasets
df_elo_raw <- bind_rows(df_elo_hist, df_elo_latest)
rm(df_elo_latest, df_elo_hist)

# Filter games
df_elo <- df_elo_raw %>% 
  filter(
    season >= params$year_min,  # min year
    ! season %in% params$years_exclude,
    is.na(playoff)  # remove playoff weeks
  )

# Remove duplicates (possible if between seasons and current season is also in historic data)
df_elo <- df_elo %>% distinct()

# Calculate week for each game
df_elo <- df_elo %>%
  mutate(
    season = as.integer(season),
    date_rounded = floor_date(date, unit = "week", week_start = 3)
  ) %>%
  group_by(season) %>%
  mutate(
    week = dense_rank(date_rounded)
  ) %>%
  ungroup()

# Clean team names
df_elo <- df_elo %>%
  left_join(
    df_teams %>% select(team1 = team_short, team1_master = team_master_short),
    by = "team1"
  ) %>%
  left_join(
    df_teams %>% select(team2 = team_short, team2_master = team_master_short),
    by = "team2"
  ) %>%
  mutate(
    team1_master = ifelse(is.na(team1_master), team1, team1_master),
    team2_master = ifelse(is.na(team2_master), team2, team2_master)
  ) %>%
  select(-team1, -team2) %>%
  rename(
    team_home = team1_master, 
    team_away = team2_master,
    elo_home = elo1_pre,
    elo_away = elo2_pre,
    prob_home = elo_prob1
  )

# Calculate complementary probability for team 2
df_elo <- df_elo %>%
  mutate(prob_away = 1 - prob_home)

# Select key columns
df_elo <- df_elo %>%
  select(
    season,
    week,
    date,
    team_home,
    team_away,
    elo_home,
    elo_away,
    prob_home,
    prob_away,
    score_home = score1,
    score_away = score2
  )


# EXTRACT SEASON INITIALIZATION ELO SCORES --------------------------------

# The purpose of this section is to extract the "season starting conditions", or
# in other words the starting ELO/power-ranking scores for each team at the
# start of the season.  This will be used as a starting point for simulations.


df_elo_init <- df_elo %>%
  filter(week == 1) %>%
  select(
    season,
    week,
    team_home,
    team_away,
    elo_home,
    elo_away
  ) %>%
  pivot_longer(
    cols = c(team_home, team_away, elo_home, elo_away),
    names_to = c(".value", "location"),
    names_pattern = "(.*)_(.*)",
    values_to = "val"
  ) %>%
  select(season, team, elo)

# Write to disk
write_parquet(df_elo_init, here("data", "prepared", "df_elo_init.parquet"))

# EXTRACT SUMMARY OF HISTORIC ELO DIFF AND POINT DIFF ---------------------

# The purpose of this section is to gather a reference dataset of all matchups,
# with summarized stats of elo/score differencies.  This will be used to
# generate a distribution of potential game outcomes for similar matchups.

df_elo_diff <- df_elo %>%
  select(
    season, 
    week, 
    date, 
    team_home, 
    team_away, 
    elo_home,
    elo_away,
    score_home = score1, 
    score_away = score2
  ) %>%
  mutate(
    favorite_team = ifelse(elo_home > elo_away, "home", "away"),
    favorite_elo_diff = case_when(
      favorite_team == "home" ~ elo_home - elo_away,
      favorite_team == "away" ~ elo_away - elo_home
    ),
    favorite_point_diff = case_when(
      favorite_team == "home" ~ score_home - score_away,
      favorite_team == "away" ~ score_away - score_home
    )
  )

# Write to disk
write_parquet(df_elo_diff, here("data", "prepared", "df_elo_diff.parquet"))


# EXTRACT NFL MATCHUP DATA ------------------------------------------------

# Extract schedule
df_schedule <- df_elo %>%
  select(
    season, 
    week, 
    date, 
    team_home, 
    team_away
  )

# Write to disk
write_parquet(df_schedule, here("data", "prepared", "df_schedule.parquet"))


# EXTRACT HISTORIC MATCHUP RESULTS ----------------------------------------

# Extract result data into wide format (i.e. one record per season-week-game)
df_results_wide <- df_elo %>%
  select(
    season, 
    week, 
    date, 
    team_home, 
    team_away, 
    elo_home,
    elo_away,
    score_home = score1, 
    score_away = score2
  ) %>%
  mutate(
    winner = case_when(
      score_home > score_away ~ team_home,
      score_away > score_home ~ team_away,
      score_home == score_away ~ "TIE"
    ),
    mov = abs(score_home - score_away) # margin of victory
  )

# Pivot into long format (one record per season-week-game-team)
df_results_long <- df_results_wide %>%
  pivot_longer(
    cols = c(team_home, team_away),
    names_to = "location",
    names_prefix = "team_",
    values_to = "team"
  ) %>%
  mutate(
    result = case_when(
      winner_location == location ~ "W",
      winner_location == "tie" ~ "T",
      TRUE ~ "L"
    ),
    point_diff = case_when(
      result == "W" ~ winner_margin,
      result == "L" ~ -winner_margin,
      result == "T" ~ 0
    )
  ) %>%
  select(season, week, team, point_diff, result)

# Write to disk
write_parquet(df_results, here("data", "prepared", "df_results.parquet"))


# Reorganize columns
df_elo <- df_elo %>%
  select(
    season,
    week,
    date,
    team_home,
    team_away,
    elo_home,
    elo_away,
    prob_home,
    prob_away
  )

# Save data
write_parquet(df_elo, here("data", "prepared", "df_elo.parquet"))


# EXTRACT NFL SEASON SCHEDULE ---------------------------------------------

df_schedule <- df_elo %>%

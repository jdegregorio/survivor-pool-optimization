# The purpose of this script is to cacluate the win probability of future
# matchups using ELO ratings generated from historical matchups.

# SETUP -----------------------------------------------------------------------

# Load packages
library(dplyr)
library(tidyr)
library(here)
library(feather)
library(readr)

# Source functions
source(here("code", "funs_elo.R"))

# Load data
df.games <- feather::read_feather(here::here("data", "output", "games.feather"))
lu.teams <- readr::read_csv(here::here("data", "lookup", "team_lookup.csv"))

# Define parameters
factor.k = 20.0        # ELO adjustment rate
revert = 1/3    # Portion of ELO retained between seasons


# INITIALIZE ELO DATA ---------------------------------------------------------


# Initialize team ELO
teams.master <- lu.teams$team_master_short %>% unique() %>% sort()
elo <- rep(1300, length(teams.master))
names(elo) <- teams.master

# Master names for franchises that have changed location
df.elo <- df.games %>%
  left_join(
    lu.teams %>% select(team1 = team_full, team1_master = team_master_short), 
    by = "team1"
  ) %>%
  left_join(
    lu.teams %>% select(team2 = team_full, team2_master = team_master_short), 
    by = "team2"
  ) %>%
  mutate(team1 = team1_master, team2 = team2_master) %>%
  select(-team1_master, -team2_master) %>%
  mutate(
    result = case_when(
      points_team1 > points_team2 ~ 1.0,
      points_team1 < points_team2 ~ 0.0,
      points_team1 == points_team2 ~ 0.5
    ),
    elo_team1 = NA,
    elo_team2 = NA,
    prob_team1 = NA,
    prob_team2 = NA
  ) %>%
  arrange(date, hour)


# CALCULATE ELO ---------------------------------------------------------------

# Look through games
for (i in 1:nrow(df.elo)) {
  
  # Get teams
  team1 <- df.elo$team1[[i]]
  team2 <- df.elo$team2[[i]]
  
  # Get scores
  points.team1 <- df.elo$points_team1[[i]]
  points.team2 <- df.elo$points_team2[[i]]
  
  # Get result
  result <- df.elo$result[[i]]
  
  # Get locations
  location.team1 <- df.elo$location_team1[[i]]
  location.team2 <- df.elo$location_team2[[i]]
  
  # Determine home team
  team.home <- case_when(
    location.team1 == "home" ~ "team1",
    location.team2 == "home" ~ "team2",
    TRUE ~ ""
  )
  
  # Get latest elo
  elo.team1 <- elo[team1]
  elo.team2 <- elo[team2]
  elo.diff <- elo.team1 - elo.team2
  elo.diff.winner <- ifelse(result == 1, elo.diff, -elo.diff)
  
  # Calculate probabilities
  prob.team1 <- elo_prob(elo.team1, elo.team2, team.home)
  prob.team2 <- 1 - prob.team1
  
  # Assign values to dataframe
  df.elo$elo_team1[[i]] <- elo.team1
  df.elo$elo_team2[[i]] <- elo.team2
  df.elo$prob_team1[[i]] <- prob.team1
  df.elo$prob_team2[[i]] <- prob.team2
  
  # Update elo ratings (if game is complete)
  if (!is.na(result)) {
    factor.delta <- result - prob.team1
    factor.mov <- log(abs(points.team1 - points.team2) + 1) * (2.2 / (elo.diff.winner*0.001 + 2.2))
    elo.change <- factor.k * factor.delta * factor.mov
    
    elo[team1] <- elo[team1] + elo.change
    elo[team2] <- elo[team2] - elo.change
  }
  
}

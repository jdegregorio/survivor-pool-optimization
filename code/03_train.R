# The purpose of this script is to train survival pool agents via many rounds of
# simulated survival pool seasons.  The agents will be in competition with
# themselves.

# Strategy/psudo-code:
#  - Spawn population of n agents in a survivor pool
#  - Simulate survivor pool/season
#    - Initialize a season by randomly selecting a historic NFL season schedule, and initial ELO ratings for each team.
#    - Week-to-week routine
#      - Prepare feature data
#        - Current week stats (ELO scores of each team, win prob of each matchup)
#        - Future schedule/matchup states (W1-W16 win prob per team)
#        - Past competitor agent picks (remaining teams to pick from, etc.)
#      - Agents make picks selections for week
#      - Simulate game outcomes
#      - Apply survivor pool rules (i.e. eliminate agents, advance agents)
#      - Adjust ELO scores of teams
#      - Advance to next week
#  - Calculate fitness of each agent
#  - Update model weights based on fitness (natural selection step)
#  - Repeat for N generations

# SETUP -----------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))


# LOAD DATA ---------------------------------------------------------------

df_elo_init <- read_parquet(here("data", "prepared", "df_elo_init.parquet"))
df_schedule <- read_parquet(here("data", "prepared", "df_schedule.parquet"))
df_elo_diff <- read_parquet(here("data"< "prepared", "df_elo_diff.parquet"))


# INITIALIZE --------------------------------------------------------------



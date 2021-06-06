# The purpose of this script is to evaluate baseline performance measures for
# survivor pools.

# Baseline Strategies:
#  - Naive:  Random(ish) pick selection based on pick distributions
#  - Group-Think:  Pick the available team with the greatest weekly pick distribution
#  - Expected Value: Pick the available team with the greatest expected value


# SETUP -------------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Source packages and functions
source("./code/packages.R")

# Load parameters
params <- read_yaml(here("code", "params.yaml"))

# Setup parallel processing
plan(multisession, workers = 6)
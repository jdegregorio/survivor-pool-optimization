
#' Clean ELO Data
#'
#' @param data dataframe, raw elo data
#'
#' @return dataframe, cleaned elo data
#' @export
#'
clean_elo <- function(data) {
  
  # Clean data
  data <- data %>%
    mutate(
      season = as.integer(season),
      date_rounded = round_date(date, unit = "week")
    ) %>%
    group_by(season) %>%
    mutate(
      week = dense_rank(date_rounded)
    ) %>%
    ungroup() %>%
    left_join(
      lu.teams %>% select(team1 = team_short, team1_master = team_master_short),
      by = "team1"
    ) %>%
    left_join(
      lu.teams %>% select(team2 = team_short, team2_master = team_master_short),
      by = "team2"
    ) %>%
    mutate(
      team1_master = ifelse(is.na(team1_master), team1, team1_master),
      team2_master = ifelse(is.na(team2_master), team2, team2_master)
    ) %>%
    select(-team1, -team2) %>%
    rename(
      team1 = team1_master, 
      team2 = team2_master,
      elo_team1 = elo1,
      elo_team2 = elo2,
      prob_team1 = elo_prob1
    ) %>%
    mutate(prob_team2 = 1 - prob_team1) %>%
    select(
      season,
      week,
      date,
      team1,
      team2,
      elo_team1,
      elo_team2,
      prob_team1,
      prob_team2
    )
  
  return(data)
}


#' Clean Pick Distribution Data
#'
#' @param data dataframe, raw pick distribution data
#'
#' @return dataframe, clean pick distribution data
#' @export
#'
clean_pickdist <- function(data) {
  
  data <- data %>%
    mutate(
      team = str_extract(team, "[:upper:]{2,3}"),
      pick_pct = str_remove_all(pick_pct, "\\%") %>% as.numeric() / 100
    ) %>%
    left_join(
      lu.teams %>% select(team = team_short, team_master = team_master_short),
      by = "team"
    ) %>%
    mutate(team = team_master) %>%
    select(-team_master) %>%
    select(season, week, team, pick_pct)
  
  return(data)
}


#' Calculated the expected value for each team for given season/week
#'
#' @param ftr_season integer, season
#' @param ftr_week integer, week
#' @param data_metrics dataframe, containing team, win_prob, and pick_pct for each team/week/season
#'
#' @return dataframe, containing table of each team with expected value
#' @export
#'
calculate_expected_value <- function(ftr_season, ftr_week, data_metrics) {
  
  # Initialize weekly data
  tmp.metrics <- data_metrics %>% filter(season == ftr_season, week == ftr_week)
  game.count <- max(tmp.metrics$game_id)
  
  # Create grid of possible game outcomes
  df.outcomes <- 
    gtools::permutations(2, game.count, v = c("team1", "team2"), repeats.allowed = TRUE) %>%
    as_tibble() %>%
    mutate(scenario_id = 1:n()) %>%
    pivot_longer(-scenario_id, names_to = "game_id", values_to = "winner") %>%
    mutate(game_id = str_sub(game_id, 2) %>% as.integer()) %>%
    left_join(
      tmp.metrics %>% select(game_id, team_id, team, win_prob, pick_pct), 
      by = c("game_id" = "game_id", "winner" = "team_id")
    )
  
  # Calculate terms for EV analysis
  df.terms <- df.outcomes %>%
    select(-winner) %>%
    pivot_wider(
      id_cols = scenario_id, 
      names_from = team, 
      values_from = c(win_prob, pick_pct),
      values_fill = list(win_prob = 1, pick_pct = 0)
    ) %>%
    mutate(
      prob_scenario = matrixStats::rowProds(as.matrix(select(., starts_with("win_prob")))),
      entry_value = 1 / rowSums(as.matrix(select(., starts_with("pick_pct")))),
      term = prob_scenario * entry_value
    ) %>%
    select(scenario_id, prob_scenario, entry_value, term)
  
  # Summarize expected value by team for specific week
  df.summary <- df.outcomes %>%
    select(scenario_id, game_id, team) %>%
    left_join(df.terms, by = "scenario_id") %>%
    group_by(team) %>%
    summarize(expected_value = sum(term)) %>%
    ungroup() %>%
    arrange(desc(expected_value))
  
  return(df.summary)
}
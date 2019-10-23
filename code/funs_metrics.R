

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
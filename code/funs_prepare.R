
#' Calculated the expected value for each team for given season/week
#'
#' @param ftr_season integer, season
#' @param ftr_week integer, week
#' @param data_metrics dataframe, containing team, prob, and pick_pct for each team/week/season
#'
#' @return dataframe, containing table of each team with expected value
#' @export
#'
calculate_expected_value <- function(ftr_season, ftr_week, data_metrics) {
  
  # Initialize weekly data
  tmp_metrics <- data_metrics %>% 
    filter(season == ftr_season, week == ftr_week) %>% 
    select(game_id, team, location, prob, pick_pct)
  
  game_count <- max(tmp_metrics$game_id)
  
  # Create grid of possible game outcomes
  df_outcomes <- 
    gtools::permutations(2, game_count, v = c("home", "away"), repeats.allowed = TRUE) %>%
    as_tibble() %>%
    mutate(scenario_id = 1:n()) %>%
    pivot_longer(-scenario_id, names_to = "game_id", values_to = "winner_location") %>%
    mutate(game_id = str_sub(game_id, 2) %>% as.integer()) %>%
    left_join(
      tmp_metrics, 
      by = c("game_id" = "game_id", "winner_location" = "location")
    )
  
  # Calculate terms for EV analysis
  df_terms <- df_outcomes %>%
    select(-winner_location) %>%
    pivot_wider(
      id_cols = scenario_id, 
      names_from = team, 
      values_from = c(prob, pick_pct),
      values_fill = list(prob = 1, pick_pct = 0)
    ) %>%
    mutate(
      prob_scenario = matrixStats::rowProds(as.matrix(select(., starts_with("prob")))),
      entry_value = 1 / rowSums(as.matrix(select(., starts_with("pick_pct")))),
      term = prob_scenario * entry_value
    ) %>%
    select(scenario_id, prob_scenario, entry_value, term)
  
  # Summarize expected value by team for specific week
  df_summary <- df_outcomes %>%
    select(scenario_id, game_id, team) %>%
    left_join(df_terms, by = "scenario_id") %>%
    group_by(team) %>%
    summarize(expected_value = sum(term)) %>%
    ungroup() %>%
    arrange(desc(expected_value))
  
  return(df_summary)
}

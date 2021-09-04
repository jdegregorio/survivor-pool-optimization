#' Predict Win Probability from ELO
#'
#' @param elo_team1 the elo score for team 1
#' @param elo_team2 the elo score for team 2
#'
#' @return the estimated win probability for team 1
#' @export
predict_win_prob <- function(elo_team1, elo_team2) {
  1 / (10^(-(elo_team1 - elo_team2) / 400) + 1) 
}



#' Calculate ELO Adjustments
#'
#' A function for determine the ELO adjustment from after the game outcome. The
#' resulting adjustment should be added to team1, and subtracted from team 2
#'
#' @param elo_team1 pregame elo rating of team 1
#' @param elo_team2 pregame elo rating of team 2
#' @param score_team1 score of team 1
#' @param score_team2 score of team 2
#' @param k elo tuning parameter, recommended to be 20 for NFL by FiveThirtyEight
#'
#' @return numeric, the adjustment amount to be added to team1 and subtracted from team2
#' @export
calculate_elo_adjustment <- function(elo_team1, elo_team2, score_team1, score_team2, k = 20) {
  
  # Calculate outcome delta factor
  outcome <- dplyr::case_when(
    score_team1 > score_team2 ~ 1,
    score_team1 == score_team2 ~ 0.5,
    score_team1 < score_team2 ~ 0
  )
  outcome_delta_factor <- outcome - predict_win_prob(elo_team1, elo_team2)
  
  # Calculate margin of victory multiplier factor
  winner_elo_diff <- dplyr::case_when(
    outcome == 1 ~ elo_team1 - elo_team2,
    outcome == 0 ~ elo_team2 - elo_team1
  )
  mov_mult_factor <- log(abs(score_team1 - score_team2) + 1) * (2.2 / (winner_elo_diff * 0.001 + 2.2))

  elo_change <- k * outcome_delta_factor * mov_mult_factor
  elo_change <- ifelse(is.na(elo_change), 0, elo_change)
  return(elo_change)
}

#' Simulate Game Score Differential by ELO Ratings
#'
#' @param elo_favorite elo of favorite team
#' @param elo_underdog elo of underdog team
#' @param data_elo_diff prepared dataframe with historic elo data (df_elo_diff)
#' @param k_nearest the number of nearest neighbors to sample from
#' @param weight_factor factor for how much distance-based weighting is applied (0 if for kth neighbor to have zero percent change of sampling, 1 to reduce weighting by half, and so on
#'
#' @return a single sample of favorite score differential
#' @export
simulate_score_diff <- function(elo_favorite, elo_underdog, data_elo_diff, k_nearest = 100, weight_factor = 1) {
  
  data_elo_diff %>%
    mutate(
      elo_favorite = case_when(
        favorite_team == "home" ~ elo_home,
        favorite_team == "away" ~ elo_away
      ),
      elo_underdog = case_when(
        favorite_team == "home" ~ elo_away,
        favorite_team == "away" ~ elo_home
      ),
      elo_diff_dist = favorite_elo_diff_target - favorite_elo_diff, 
      elo_favorite_dist = elo_favorite_target - elo_favorite,
      elo_underdog_dist = elo_underdog_target - elo_underdog,
      total_dist = sqrt(elo_diff_dist^2 + elo_favorite_dist^2 + elo_underdog_dist^2)
    ) %>%
    select(elo_favorite, elo_underdog, total_dist, favorite_point_diff) %>%
    slice_min(order_by = total_dist, n = k_nearest) %>%
    mutate(
      weight = (total_dist - min(total_dist)) / (max(total_dist) - min(total_dist)),
      weight = abs(weight - 1) + weight_factor
    ) %>%
    slice_sample(n = 1, weight_by = weight) %>%
    pull(favorite_point_diff)
  
}


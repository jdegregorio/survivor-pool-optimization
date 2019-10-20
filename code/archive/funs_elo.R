
#' Calculate win probability based on ELO
#'
#' @param elo_team1 numeric, team 1 elo
#' @param elo_team2 numeric, team 2 elo
#' @param team1_home logical, assign TRUE if team 1 is at home
#' @param hfa numeric, home field advantage elo bonus
#'
#' @return numeric, win probability for team 1
#' @export
#'
elo_prob <- function(elo_team1, elo_team2, home_team = "", hfa = 60) {
  
  # Determine home field advantage
  hfa <- case_when(
    home_team == "team1" ~ hfa,
    home_team == "team2" ~ -hfa,
    TRUE ~ 0
  )
  
  # Calculate ELO Difference
  elo_diff <- elo_team1 - elo_team2 + hfa
  
  # Calculate win probability
  elo = 1 / (10^(-elo_diff/400) + 1)
  
  return(elo)
}

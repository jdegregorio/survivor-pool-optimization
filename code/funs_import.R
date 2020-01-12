
#' Scrape Pick Distribution Data
#' 
#' The purpose of this function is to scrape the pick dstribution data from
#' survivorgrid.com. This website contains historical pick distribution data for
#' survivor pools.
#'
#' @param season integer, year for pulling game history
#' @param week integer, week for pulling game history
#' @param delay integer, seconds delay after requesting page
#'
#' @return dataframe, containing pick distribution data for selected season
#' @export
#'
scrape_pickdist <- function(season, week, delay = 3) {
  
  # Construct url
  url <- paste0("https://www.survivorgrid.com/", season, "/", week)
  
  # Load raw results
  data <-
    url %>%
    xml2::read_html(options = "RECOVER") %>%
    rvest::html_node("#grid") %>%
    rvest::html_table() %>%
    select(
      team = Team,
      pick_pct = `P%`
    ) %>%
    as_tibble()
  
  # Delay (optional)
  Sys.sleep(delay)
  
  return(data)
}
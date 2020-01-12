
#' Get the current NFL season year
#'
#' @return integer, year of current season (i.e. the year the season starts)
#' @export
#'
get_current_season <- function () {
  if_else(
    month(Sys.Date()) >= 8,  # check month (i.e. season starts in September)
    year(Sys.Date()),        # use current year
    year(Sys.Date()) - 1     # use previous year
  )
}
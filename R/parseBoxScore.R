#' Function to convert box-score JSON data from ESPN.com game page to named list
#'
#' @param box_info JSON data from game page on ESPN.com
#'
#' @return Named list of statistics in box score
parseBoxScore <- function(box_info) {
  stat_vals <- lapply(box_info$statistics, function(x){x$displayValue})
  names(stat_vals) <- lapply(box_info$statistics, function(x){x$name})

  stat_vals$team_id <- box_info$team$id
  stat_vals$team_abbr <- box_info$team$abbreviation

  stat_vals
}

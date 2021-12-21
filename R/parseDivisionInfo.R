#' Convert ESPN.com JSON response to a useable dataframe
#'
#' @param div_json Schedule for one NCAA FB division (FBS or FCS)
#'
#' @return Dataframe with one row of data per game
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#'
parseDivisionInfo <- function(div_json) {
  div_json %>%
    map_dfr(parseDateGames)
}

parseDateGames <- function(date_json) {
  date_json$games %>%
    map_dfr(parseGameInfo)
}

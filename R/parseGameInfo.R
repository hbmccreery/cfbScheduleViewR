#' Convert JSON of game info from ESPN.com to a tibble
#'
#' @param game_json JSON data scraped from the ESPN.com schedule
#'
#' @return One-row tibble with information about game
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom glue glue
parseGameInfo <- function(game_json) {
  # hopefully competitions is only len 1??
  comp_info <- game_json$competitions[[1]]
  teams <- comp_info$competitors

  broadcasts <- comp_info$broadcasts
  is_espn_plus <- ifelse(
    length(broadcasts) == 0,
    F,
    ('ESPN+' %in% comp_info$broadcasts[[1]]$names)
  )
  broadcasts <- if(length(broadcasts) == 0) {NA} else {broadcasts}

  tibble(
    date = as.POSIXct(game_json$date, format = '%Y-%m-%dT%H:%M', tz="UTC"),
    name_short = game_json$shortName,
    name = game_json$name,
    id = game_json$id,
    venue_city = comp_info$venue$address$city,
    venue_state = comp_info$venue$address$state,
    venue_name = comp_info$venue$fullName,
    broadcast_info = broadcasts,
    is_espn_plus = is_espn_plus,
    home_id = teams[[1]]$id,
    home_score = teams[[1]]$score,
    home_abbr = teams[[1]]$team$abbreviation,
    home_name = teams[[1]]$team$location,
    home_logo = teams[[1]]$team$logo,
    home_records = length(teams[[1]]$records),
    home_home = teams[[1]]$homeAway,
    home_color = teams[[1]]$team$color,
    home_conf = teams[[1]]$team$conferenceId,
    home_rank = teams[[1]]$curatedRank$current,
    away_id = teams[[2]]$id,
    away_score = teams[[2]]$score,
    away_name = teams[[2]]$team$location,
    away_abbr = teams[[2]]$team$abbreviation,
    away_logo = teams[[2]]$team$logo,
    away_records = length(teams[[2]]$records),
    away_home = teams[[2]]$homeAway,
    away_color = teams[[2]]$team$color,
    away_conf = teams[[2]]$team$conferenceId,
    away_rank = teams[[2]]$curatedRank$current,
    period = game_json$status$period,
    clock = game_json$status$clock,
    status_code = game_json$status$type$name,
    status_detail = game_json$status$type$detail,
    status_complete = game_json$status$type$completed,
    espn_link = glue("https://www.espn.com/college-football/game?gameId={game_json$id}"),
    gop_link = glue("https://gameonpaper.com/cfb/game/{game_json$id}"),
    potential_espn_plus_url = glue("https://www.espn.com/watch/player/_/gameId/{id}/sourceLang/en/om-navmethod/espn%3Acollege-football%3Aschedule/eventCalendarId/{id}")
  )
}

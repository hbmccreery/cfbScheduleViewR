#' Function to convert JSON data for one play to a tibble
#'
#' @param play_json JSON data of one play scraped from the game page on ESPN.com
#'
#' @return One-row tibble with information about that specific play
#'
#' @export
#'
#' @importFrom tibble tibble
parsePlayData <- function(play_json) {
  tibble(
    play_id = play_json$playId,
    play_desc = play_json$play$text,
    period = play_json$play$period$number,
    down = play_json$play$start$down,
    distance = play_json$play$start$distance,
    yard_line = play_json$play$start$yardLine,
    home_score = play_json$play$homeScore,
    away_score = play_json$play$awayScore,
    time_left = play_json$secondsLeft,
    home_wp = play_json$homeWinPercentage
  )
}

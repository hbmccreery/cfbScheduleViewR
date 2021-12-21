#' Convert JSON of gambling odds to tibble
#'
#' @param odds_json JSON of gambling odds from ESPN.com game page
#'
#' @return One-row tibble with gambling odds from one provider
#'
#' @export
#'
#' @importFrom tibble tibble
parsePreGameOdds <- function(odds_json) {
  tibble(
    source_name = odds_json$provider$name,
    source_id = odds_json$provider$id,
    over_under = odds_json$overUnder,
    home_id = odds_json$homeTeamOdds$teamId,
    home_ml = odds_json$homeTeamOdds$moneyLine,
    home_spread_odds = odds_json$homeTeamOdds$spreadOdds,
    home_spread = odds_json$spread
  )
}

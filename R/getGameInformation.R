#' Function to retrieve data for a particular game from ESPN.com
#'
#' @param game_id ID of game to be retrieved. Long integer, not entirely sure how they come up with these.
#'
#' @return One-row tibble with game_id along with dataframes with information on gambling odds, win probabilities, and box score information
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom glue glue
#' @importFrom httr GET user_agent content
#' @importFrom jsonlite parse_json
#' @importFrom tibble tibble
getGameInformation <- function(game_id) {
  # Let's set user agent to a super common one -- they won't let me scrape if I don't!
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

  game_url <- glue("https://www.espn.com/college-football/game?gameId={game_id}&xhr=1")
  game_response <- GET(game_url, user_agent(ua))
  game_json <- parse_json(content(game_response, "text"))

  odds_df <- game_json$gamepackageJSON$pickcenter %>% map_dfr(parsePreGameOdds)
  wp_df <- game_json$gamepackageJSON$winprobability %>% map_dfr(parsePlayData)
  box_df <- game_json$gamepackageJSON$boxscore$teams %>% map_dfr(parseBoxScore)

  tibble(
    id = game_id,
    odds_df = list(odds_df),
    wp_df = list(wp_df),
    box_df = list(box_df)
  )
}

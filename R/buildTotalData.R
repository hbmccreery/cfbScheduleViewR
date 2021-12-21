#' Function to build data frame with all information
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr rename mutate arrange n select left_join case_when contains distinct pull
#' @importFrom tidyr separate
#' @importFrom stringr str_trim str_replace
#' @importFrom purrr map_dfr
#' @importFrom glue glue
buildTotalData <- function(fbs_json, fcs_json) {
  ORDERED_GAME_STATUSES <- c(
    "STATUS_IN_PROGRESS",
    "STATUS_HALFTIME",
    "STATUS_END_PERIOD",
    "STATUS_DELAYED",
    "STATUS_SCHEDULED",
    "STATUS_FINAL"
  )

  schedule_dates <- unique(c(
    names(fbs_json$content$schedule),
    names(fcs_json$content$schedule)
  ))

  rivalries_to_match <- cfbScheduleViewR::rivalries %>%
    rename(
      home_team = .data$team_a,
      away_team = .data$team_b
    ) %>%
    rbind(
      rivalries %>%
        rename(
          home_team = .data$team_b,
          away_team = .data$team_a
        )
    )

  total_schedule <- parseDivisionInfo(fbs_json$content$schedule) %>%
    rbind(parseDivisionInfo(fcs_json$content$schedule)) %>%
    distinct()

  total_game_df <- total_schedule %>%
    pull(id) %>%
    map_dfr(getGameInformation)

  if(dim(total_game_df)[1] > 1) {
    game_wp_stats <- total_game_df$wp_df %>% map_dfr(getGameWinProbSummary)
    game_wp_stats$id <- total_game_df$id

    game_odds_df <- total_game_df$odds_df %>% map_dfr(getGameOddsSummary)
  } else {
    game_wp_stats <- tibble(id=NA)
    game_odds_df <- tibble(id=NA)
  }

  in_progress_status_codes <- c(
    "STATUS_IN_PROGRESS",
    "STATUS_END_PERIOD",
    "STATUS_HALFTIME"
  )

  non_cfbfastr_conf <- tibble(
    id = c(29, 27, 40, 22, 21, 175, 24, 28, 26, 20, 31, 135, 30),
    abbreviation = c("SoCon", "Pat", "MAAC", "Ivy", "MVC", "?", "MEAC", "BSth", "OVB", "BSky", "MEAC", "RMAC", "SLand"),
    name = "",
    short_name = "",
  )

  total_conf_info <- cfbfastR::cfbd_conf_types_df %>%
    rbind(non_cfbfastr_conf)

  DAY_WINDOW_LEVELS <- c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday Morning",
    "Saturday Afternoon",
    "Saturday Night",
    "real sicko hours who's up"
  )

  combined_df <- createScheduleData(
    total_schedule,
    game_wp_stats,
    game_odds_df,
    total_sp_plus,
    total_conf_info,
    rivalries_to_match
  )
}

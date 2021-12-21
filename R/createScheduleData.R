#' Function to create data the table is built on
#'
#' @param total_schedule Data frame with schedule data from ESPN.com
#' @param game_wp_stats Data frame of game-level win probabilities from ESPN.com
#' @param game_odds_df Data frame of pre-game betting odds from ESPN.com
#' @param total_sp_plus Data frame of all SP+ rankings
#' @param total_conf_info Data frame of conference abbreviations, partially from `cfbFastR` and partially manually scraped
#' @param rivalries_to_match Data frame of rivalries scraped from wikipedia
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr left_join mutate case_when distinct select arrange
#' @importFrom stringr str_replace
#' @importFrom glue glue
createScheduleData <- function(
  total_schedule,
  game_wp_stats,
  game_odds_df,
  total_sp_plus,
  total_conf_info,
  rivalries_to_match
) {
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

  ORDERED_GAME_STATUSES <- c(
    "STATUS_IN_PROGRESS",
    "STATUS_HALFTIME",
    "STATUS_END_PERIOD",
    "STATUS_DELAYED",
    "STATUS_SCHEDULED",
    "STATUS_FINAL"
  )

  IN_PROGRESS_STATUS_CODES <- c(
    "STATUS_IN_PROGRESS",
    "STATUS_END_PERIOD",
    "STATUS_HALFTIME"
  )

  DIVISION_LEVELS <- c(
    "FBS",
    "FBS/FCS",
    "FCS"
  )

  combined_df <- total_schedule %>%
    left_join(game_wp_stats) %>%
    left_join(game_odds_df) %>%
    left_join(
      total_sp_plus %>%
        select(
          home_name=.data$team,
          home_div=.data$division,
          home_sp=.data$sp_plus,
          home_sp_rk=.data$div_sp_rk,
          home_sp_off=.data$off_sp,
          home_sp_off_rk=.data$off_rk,
          home_sp_def=.data$def_sp,
          home_sp_def_rk=.data$def_rk
        )
    ) %>%
    left_join(
      total_sp_plus %>%
        select(
          away_name=.data$team,
          away_div=.data$division,
          away_sp=.data$sp_plus,
          away_sp_rk=.data$div_sp_rk,
          away_sp_off=.data$off_sp,
          away_sp_off_rk=.data$off_rk,
          away_sp_def=.data$def_sp,
          away_sp_def_rk=.data$def_rk
        )
    ) %>%
    left_join(
      total_conf_info %>%
        mutate(
          home_conf = as.character(.data$id),
          home_conf_abbr = .data$abbreviation
        ) %>%
        select(contains("home_"))
    ) %>%
    left_join(
      total_conf_info %>%
        mutate(
          away_conf = as.character(.data$id),
          away_conf_abbr = .data$abbreviation
        ) %>%
        select(contains("away_"))
    ) %>%
    left_join(
      rivalries_to_match,
      by = c("home_name" = "home_team", "away_name" = "away_team")
    ) %>%
    mutate(
      status_fct = factor(.data$status_code, levels = ORDERED_GAME_STATUSES),
      game_loc = glue("{venue_city}, {venue_state}"),
      clock_min = case_when(
        .data$clock < 60 ~ glue(""),
        T ~ glue("{clock %/% 60}")
      ),
      clock_sec = case_when(
        (.data$clock %% 60) == 0 ~ glue("00"),
        (.data$clock %% 60) < 10 ~ glue('0{clock %% 60}'),
        T ~ glue("{clock %% 60}")
      ),
      display_clock = case_when(
        .data$status_code == "STATUS_FINAL" ~ glue('FINAL'),
        .data$status_code == "STATUS_SCHEDULED" ~ glue('---'),
        T ~ glue("Q{period} {clock_min}:{clock_sec}")
      ),
      ## TODO: CHANGE TO case_when
      away_rank_disp = ifelse(
        .data$away_rank > 26,
        '',
        paste0('#', as.character(.data$away_rank))
      ),
      home_rank_disp = ifelse(
        .data$home_rank > 26,
        '',
        paste0('#', as.character(.data$home_rank))
      ),
      spread_disp = case_when(
        is.na(.data$home_spread) ~ "?",
        .data$home_spread > 0 ~ paste0("+", as.character(.data$home_spread)),
        T ~ as.character(.data$home_spread)
      ),
      home_ml_disp = case_when(
        is.na(.data$home_ml) ~ "?",
        T ~ as.character(.data$home_ml)
      ),
      display_game = glue("{away_rank_disp} {away_name} @ {home_rank_disp} {home_name} ({home_spread})"),
      sp_pt_total = ((.data$home_sp_off + .data$away_sp_def) / 2) + ((.data$home_sp_def + .data$away_sp_off) / 2),
      sp_home_spread = .data$away_sp - (.data$home_sp + 2.5),  # home == 2.5
      sp_away_score = (.data$sp_pt_total + .data$sp_home_spread) / 2,
      sp_home_score = .data$sp_pt_total - .data$sp_away_score,
      display_score = case_when(
        status_code == "STATUS_SCHEDULED" ~ glue("{round(sp_away_score, 1)} - {round(sp_home_score, 1)}"),
        T ~ glue("{away_score} - {home_score}")
      ),
      kick_time = format(as.POSIXct(.data$date), tz="EST", usetz=T, "%I:%M %p"),
      date_format = format(as.POSIXct(.data$date), tz="EST", usetz=T, "%a %I:%M"),
      weekday_format = str_replace(format(.data$date, tz="EST", usetz=T, "%A"), " EST", ""),
      actual_game_date = as.Date(str_replace(format(.data$date, tz="EST", usetz=T, "%Y-%m-%d"), " EST", "")),
      espn_plus_link = ifelse(.data$is_espn_plus, .data$potential_espn_plus_url, ''),
      score_diff = case_when(
        (.data$home_score == "0") & (.data$away_score == "0") & .data$period < 2 ~ 100,
        T ~ abs(as.double(.data$home_score) - as.double(.data$away_score))
      ),
      clock_rem = .data$clock + ((4 - pmax(.data$period, 4)) * 15 * 60),
      interest_score = case_when(
        .data$period == 0 ~ 0,
        .data$status_code %in% IN_PROGRESS_STATUS_CODES ~ (
          sqrt(.data$score_diff + 1) * (.data$clock_rem ^ 2)
        ),
        T ~ 0
      ),
      conf_display = case_when(
        .data$away_conf == .data$home_conf ~ glue("{home_conf_abbr} conference game"),
        .data$away_conf != .data$home_conf ~ glue("OOC {away_conf_abbr} @ {home_conf_abbr}")
      ),
      loc_disp = glue("{venue_name}<br>{venue_city}, {venue_state}"),
      team_disp = glue("{away_rank_disp} {away_name} @ {home_rank_disp} {home_name}"),
      spread_sign = case_when(
        .data$home_spread < 0 ~ "-",
        T ~ "+"
      ),
      spread_disp = case_when(
        is.na(.data$home_spread) ~ glue(""),
        T ~ glue("{home_abbr} {spread_sign}{abs(home_spread)}")
      ),
      ml_disp = case_when(
        is.na(.data$home_ml) ~ glue(""),
        T ~ glue(" | {home_abbr} {case_when(home_ml > 0 ~ '+', T ~ '-')}{abs(home_ml)}")
      ),
      ou_disp = case_when(
        is.na(.data$over_under) ~ glue(""),
        T ~ glue(" | O/U {over_under}")
      ),
      odds_disp = paste0(.data$spread_disp, .data$ml_disp, .data$ou_disp),
      rivalry_disp = case_when(
        is.na(.data$rivalry_game) ~ glue(""),
        T ~ glue("<b><em>{meetings}th {rivalry_game}</em></b>")
      ),
      # disp_str = case_when(
      #   .data$odds_disp == "" & .data$rivalry_disp == "" ~ paste0("<b>", .data$team_disp, "</b>", "<br>", .data$loc_disp),
      #   .data$rivalry_disp == "" ~ paste0("<b>", .data$team_disp, "</b>", "<br>", .data$loc_disp, "<br>", .data$odds_disp),
      #   .data$odds_disp == "" ~ paste0("<b>", .data$team_disp, "</b>", "<br>", .data$loc_disp, "<br>", .data$rivalry_disp),
      #   T ~ paste0("<b>", .data$team_disp, "</b>", "<br>", .data$odds_disp, "<br>", .data$loc_disp, "<br>", .data$rivalry_disp)
      # ),
      disp_str = case_when(
        odds_disp == "" & rivalry_disp == "" ~ paste0("<b>", team_disp, "</b>"),
        rivalry_disp == "" ~ paste0("<b>", team_disp, "</b>", "<br>", odds_disp),
        odds_disp == "" ~ paste0("<b>", team_disp, "</b>", "<br>", rivalry_disp),
        T ~ paste0("<b>", team_disp, "</b>", "<br>", odds_disp, "<br>", rivalry_disp)
      ),
      home_sp_disp = paste(
        glue("#{home_sp_off_rk} OFF"),
        glue("#{home_sp_def_rk} DEF"),
        sep = "<br>"
      ),
      away_sp_disp = paste(
        glue("#{away_sp_off_rk} OFF"),
        glue("#{away_sp_def_rk} DEF"),
        sep = "<br>"
      ),
      tab_disp = paste0(.data$date_format, " ", .data$team_disp, " (", .data$conf_display, ")"),
      game_hour_start = as.integer(
        str_replace(
          format(.data$date, tz="EST", usetz=T, "%H"),
          " EST",
          ""
        )
      ),
      day_time_bucket = factor(
        case_when(
          .data$weekday_format != 'Saturday' ~ .data$weekday_format,
          .data$game_hour_start <= 12 ~ 'Saturday Morning',
          .data$game_hour_start <= 16 ~ 'Saturday Afternoon',
          .data$game_hour_start <= 21 ~ 'Saturday Night',
          T ~ "real sicko hours who's up"
        ),
        levels = DAY_WINDOW_LEVELS
      ),
      div_type = factor(
        case_when(
          home_div != away_div ~ "FBS/FCS",
          T ~ home_div
        ),
        levels = DIVISION_LEVELS
      )
    ) %>%
    arrange(
      .data$day_time_bucket,
      .data$status_fct,
      (ifelse(.data$period > 4, 1, 0)),
      (ifelse(is.na(.data$home_sp_rk), 200, .data$home_sp_rk) + ifelse(is.na(.data$away_sp_rk), 200, .data$away_sp_rk)),
      .data$interest_score
    ) %>%
    distinct(.data$id, .keep_all = T)

}

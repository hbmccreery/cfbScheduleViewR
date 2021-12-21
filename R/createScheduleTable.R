#' Function to create display table for games
#'
#' @param df Dataframe with game data to be displayed
#'
#' @return a neat `gt()`
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom dplyr arrange select group_by desc
#' @importFrom glue glue
#' @importFrom scales col_numeric viridis_pal
#' @importFrom gt gt text_transform cells_body vars web_image cols_label tab_spanner fmt_percent fmt_markdown cols_align contains tab_style data_color cell_text
createScheduleTable <- function(df, table_group_vars) {
  # ok using .data breaks it in the gt() so fuck it
  away_logo <- away_sp_def_rk <- away_sp_off_rk <- away_sp_rk <- disp_str <-
    display_clock <- display_score <- espn_link <- espn_plus_link <- gop_link <-
    home_logo <- home_sp_def_rk <- home_sp_off_rk <- home_sp_rk <- NULL

  TABLE_GROUP_VALUES <- list("day_time_bucket", "div_type", "is_priority_team", "is_priority_conf")

  df %>%
    group_by(.dots = table_group_vars) %>%
    arrange(.by_group = T) %>%
    select(
      all_of(table_group_vars),
      .data$kick_time,
      .data$disp_str,
      .data$away_sp_rk,
      .data$away_sp_off_rk,
      .data$away_sp_def_rk,
      .data$away_logo,
      .data$display_score,
      .data$home_logo,
      .data$home_sp_rk,
      .data$home_sp_off_rk,
      .data$home_sp_def_rk,
      .data$display_clock,
      .data$espn_link,
      .data$espn_plus_link,
      .data$gop_link
    ) %>%
    # select(-TABLE_GROUP_VALUES[!(TABLE_GROUP_VALUES %in% table_group_vars)]) %>%
    gt() %>%
    text_transform(
      locations = cells_body(columns=vars(home_logo, away_logo)),
      fn = function(x) {web_image(url = x, height = 60)}
    ) %>%
    cols_label(
      kick_time = '',
      disp_str = '',
      away_sp_off_rk = '',
      away_sp_def_rk = '',
      away_sp_rk = '',
      away_logo = '',
      home_logo = '',
      home_sp_rk = '',
      home_sp_def_rk = '',
      home_sp_off_rk = '',
      display_score = '',
      display_clock = '',
      espn_link = '',
      espn_plus_link = '',
      gop_link = ''
    ) %>%
    tab_spanner(
      label = "SP+",
      columns = vars(
        away_sp_def_rk,
        away_sp_off_rk,
        away_sp_rk
      )
    ) %>%
    tab_spanner(
      label = "SP+",
      columns = vars(
        home_sp_def_rk,
        home_sp_off_rk,
        home_sp_rk
      )
    ) %>%
    text_transform(
      locations = cells_body(columns = vars(espn_link)),
      fn = function(x) {
        glue::glue('<a href="{x}" target="_blank">ESPN</a>')
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = vars(gop_link)),
      fn = function(x) {
        glue::glue('<a href="{x}" target="_blank">GOP</a>')
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = vars(espn_plus_link)),
      fn = function(x) {
        ifelse(
          x == '',
          glue::glue(""),
          glue::glue('<a href="{x}" target="_blank">(ESPN+)</a>')
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(columns = contains("sp_off")),
      fn = function(x) {glue("OFF<br>{x}")}
    ) %>%
    text_transform(
      locations = cells_body(columns = contains("sp_def")),
      fn = function(x) {glue("DEF<br>{x}")}
    ) %>%
    text_transform(
      locations = cells_body(columns = contains("sp_rk")),
      fn = function(x) {glue("OVR<br>{x}")}
    ) %>%
    tab_style(
      style = list(
        cell_text(size="x-large")
      ),
      locations = cells_body(
        columns = vars(display_score, display_clock)
      )
    ) %>%
    fmt_markdown(columns = vars(disp_str)) %>%
    fmt_markdown(columns = contains("_rk")) %>%
    cols_align(
      align = "center",
      columns = contains("_rk")
    ) %>%
    cols_align(
      align = "center",
      columns = vars(disp_str)
    ) %>%
    data_color(
      columns = contains("_rk"),
      colors = col_numeric(
        palette = viridis_pal(
          option="B",
          begin=0,
          end=1,
          direction=-1
        )(10),
        domain = c(0, 35, 60, 80, 90, 100, 110, 120, 130, 140)
      )
    )
}

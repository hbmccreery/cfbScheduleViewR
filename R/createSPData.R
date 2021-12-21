#' Function to clean SP+ data to be easily joined to ESPN games. Run once a week, then save to data
#'
#' @importFrom tidyr separate
#' @importFrom dplyr n
#' @importFrom stringr str_trim
createSPData <- function() {
  cfbScheduleViewR::fbs_sp_plus %>%
    separate(
      .data$TEAM,
      into = c("total_rk_disc_", "team"),
      sep = "[.]",
      extra = "merge"
    ) %>%
    separate(
      .data$team,
      into = c("team", "record"),
      sep = "[(]",
      extra = "merge"
    ) %>%
    separate(
      .data$off_sp,
      into = c("off_sp", "disc_off"),
      sep = "[(]",
      extra = "merge"
    ) %>%
    separate(
      .data$def_sp,
      into = c("def_sp", "disc_def"),
      sep = "[(]",
      extra = "merge"
    ) %>%
    separate(
      .data$st_sp,
      into = c("st_sp", "disc_st"),
      sep = "[(]",
      extra = "merge"
    ) %>%
    arrange(-.data$sp_plus) %>%
    mutate(
      team = str_trim(.data$team),
      record = str_replace(.data$record, "[)]", ""),
      off_sp = as.double(.data$off_sp),
      def_sp = as.double(.data$def_sp),
      st_sp = as.double(.data$st_sp),
      conference = "",
      division = "FBS",
      div_sp_rk = 1:n()
    ) %>%
    arrange(-.data$off_sp) %>%
    mutate(off_rk = 1:n()) %>%
    arrange(.data$def_sp) %>%
    mutate(def_rk = 1:n()) %>%
    select(-c(contains("disc_"))) %>%
    rbind(
      cfbScheduleViewR::fcs_sp_plus %>%
        select(-c(.data$pctile, .data$sp_rk)) %>%
        arrange(-.data$sp_plus) %>%
        mutate(
          st_sp = NA,
          division = "FCS",
          div_sp_rk = 1:n()
        )
    ) %>%
    left_join(
      cfbScheduleViewR::manual_matches,
      by = c("team" = "sp_team")
    ) %>%
    left_join(
      cfbScheduleViewR::richards_crosswalk %>%
        select(
          crosswalk_team = .data$`My Team names`,
          sp_team = .data$`S&P Week 9`
        ),
      by = c("team" = "sp_team")
    ) %>%
    mutate(
      team = case_when(
        !is.na(.data$espn_team) ~ .data$espn_team,
        (
          !is.na(.data$crosswalk_team) &
            !(.data$team %in% c("Pittsburgh", "California", "Miami"))
        ) ~ .data$crosswalk_team,
        T ~ .data$team
      )
    )
}

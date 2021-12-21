#'
getGameWinProbSummary <- function(wp_df) {
  if(dim(wp_df)[1] == 0) {
    return(
      tibble(
        wp_del = 0
      )
    )
  }

  wp_df %>%
    filter(!is.na(home_wp)) %>%
    summarise(
      wp_del = max(home_wp) - min(home_wp),
    )
}

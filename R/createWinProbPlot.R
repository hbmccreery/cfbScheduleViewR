#'
createWinProbPlot <- function(game_id) {
  df <- total_game_df %>%
    filter(id == game_id) %>%
    pull(wp_df)

  if((length(df) == 0)) {
    return(ggplot())
  }

  df <- df[[1]]

  if(dim(df)[1] == 0) {
    return(ggplot())
  }

  min_time <- pmin(0, min(df$time_left))
  plt <- df %>%
    arrange(time_left) %>%
    ggplot() +
    geom_hline(yintercept = 0.5, linetype='dashed', color='red') +
    geom_path(aes(x=time_left, y=home_wp)) +
    theme_void() +
    ylim(0, 1) +
    xlim(3600, 0)

  return(plt)
}

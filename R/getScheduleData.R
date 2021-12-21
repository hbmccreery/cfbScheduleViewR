#' Function to get schedule data
#'
#' @param is_fbs Boolean value to determine if FBS/FCS schedule.
#' @param week Optional integer week of season of schedule to retrieve.
#' @param is_bowls Optional parameter for if it's bowling szn
#'
#' @return JSON returned from ESPN.com
#'
#' @importFrom glue glue
getScheduleData <- function(is_fbs, week = NA, is_bowls = F) {
  # Let's set user agent to a super common one -- they won't let me scrape if I don't!
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.90 Safari/537.36"

  div_int <- if(is_fbs) {80} else {81}
  week_str <- if(is.na(week)) {""} else {paste0("week/", week, "/")}
  sched_url <- glue("https://www.espn.com/college-football/schedule/_/{week_str}group/{div_int}&xhr=1")

  if(is_bowls) {
    sched_url <- "https://www.espn.com/college-football/schedule/_/week/1/seasontype/3/group/80&xhr=1"
  }

  sched_r <- GET(sched_url, user_agent(ua))

  sched_r %>% content("text") %>% parse_json()
}

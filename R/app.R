library(shiny)
library(shinyjqui)

scheduleApp <- function(combined_df) {
  conferences <- unique(
    c(combined_df$home_conf_abbr, combined_df$away_conf_abbr)
  )
  teams <- unique(c(combined_df$home_abbr, combined_df$away_abbr))
  TABLE_GROUP_NAMES <- list("Conference", "Division", "Time Slot", "Team")
  TABLE_GROUP_VALUES <- list("is_priority_conf", "div_type", "day_time_bucket", "is_priority_team")
  names(TABLE_GROUP_VALUES) <- TABLE_GROUP_NAMES

  CONF_FACTOR_ORDER <- c("Selected Conferences", "All Conferences")
  TEAM_FACTOR_ORDER <- c("Selected Teams", "All Teams")

  ui <- fluidPage(
    fluidRow(
      column(
        selectizeInput(
          inputId = "teamInput",
          label = "Team Priorities",
          choices = teams,
          multiple = T
        ),
        width = 3
      ),
      column(
        selectizeInput(
          inputId = "confInput",
          label = "Conference Priorities",
          choices = conferences,
          selected = c("PAC", "B12", "SEC", "B1G", "ACC", "AAC"),
          multiple = T
        ),
        width = 3
      ),
      column(
        checkboxGroupInput(
          inputId = "tableGroupSelectInput",
          label = "Groups",
          choices = TABLE_GROUP_VALUES,
          selected = c("div_type", "day_time_bucket")
        ),
        width = 2
      ),
      column(
        orderInput(
          inputId = "tableGroupOrderInput",
          label = "Group Ordering",
          items = TABLE_GROUP_VALUES
        ),
        width = 4
      )
    ),
    gt_output("scheduleTable")
  )

  server <- function(input, output, session) {
    schedule_df <- reactive({
      table_groups <- input$tableGroupOrderInput
      table_groups <- table_groups[(table_groups %in% input$tableGroupSelectInput)]

      combined_df %>%
        group_by(
          is_priority_conf = factor(
            ifelse(
              (home_conf_abbr %in% input$confInput) | (away_conf_abbr %in% input$confInput),
              "Selected Conferences",
              "All Conferences"
            ),
            levels = CONF_FACTOR_ORDER
          ),
          is_priority_team = factor(
            ifelse(
              (home_abbr %in% input$teamInput) | (away_abbr %in% input$teamInput),
              "Selected Teams",
              "All Teams"
            ),
            levels = TEAM_FACTOR_ORDER
          )
        ) %>%
        createScheduleTable(., table_group_vars = table_groups)
    })
    output$scheduleTable <- render_gt(schedule_df())
  }
  shinyApp(ui, server)
}

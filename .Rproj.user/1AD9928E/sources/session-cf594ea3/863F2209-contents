library(shiny)
library(dygraphs)
library(readr)
library(dplyr)
library(tidyr)
library(xts)
library(zoo)

# Load the data
team_stats <- read_csv("team_stats_2003_2023.csv")

# Define the teams of interest
selected_teams <- c("Green Bay Packers", "Chicago Bears", "Minnesota Vikings", "Detroit Lions")

# Define UI
ui <- fluidPage(
  titlePanel("NFC North Statistics (Last Decade)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Select Metric:",
                  choices = c("Win %" = "win_loss_perc",
                              "Point Differential" = "points_diff",
                              "Turnover %" = "turnover_pct",
                              "Score %" = "score_pct",
                              "Expected Points Total" = "exp_pts_tot",
                              "Penalty Yards" = "penalties_yds"),
                  selected = "win_loss_perc")
    ),
    mainPanel(
      dygraphOutput("nfl_dygraph")
    )
  )
)

# Define server
server <- function(input, output) {
  output$nfl_dygraph <- renderDygraph({
    df <- team_stats |> 
      filter(team %in% selected_teams, year >= 2013) |> 
      mutate(win_efficiency = win_loss_perc * mov) |>  # create win_efficiency in all cases
      select(year, team, all_of(input$metric)) |> 
      pivot_wider(names_from = team, values_from = all_of(input$metric))
    
    ts_data <- xts(df[-1], order.by = as.yearmon(df$year))
    
    dygraph(ts_data, main = paste("NFC North Statistics", names(which(c(
      win_loss_perc = "Win %",
      points_diff = "Point Differential",
      tot_yds = "Total Yards",
      win_efficiency = "Win Efficiency",
      turnover_pct = "Turnover %",
      score_pct = "Score %",
      exp_pts_tot = "Expected Points Total",
      penalties_yds = "Penalty Yards"
    ) == input$metric)), "(Last Decade)")) |> 
      dyRangeSelector() |> 
      dyOptions(
        drawPoints = TRUE,
        pointSize = 3,
        colors = unname(c(
          "Green Bay Packers" = "#203731",
          "Chicago Bears" = "#FFA400",
          "Minnesota Vikings" = "#4F2683",
          "Detroit Lions" = "#0076B6"
        )[colnames(ts_data)])
      ) |> 
      dyLegend(show = "always", width = 300) |> 
      dyHighlight(highlightCircleSize = 6, 
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE) |> 
      dyAxis("x", axisLabelFormatter = JS("function(d){ return d.getFullYear(); }")) |> 
      dyAxis("y", valueFormatter = if (input$metric == "win_loss_perc") {
        JS("function(x) {return (x * 100).toFixed(1) + '%';}")
      } else {
        NULL
      })
  })
}
# Run the app
shinyApp(ui = ui, server = server)
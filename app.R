library(shiny)
library(ggplot2)
source("db_connect.r")

state_choices <- get_state_choices()
date_limits <- get_date_limits()

if (length(state_choices) == 0) {
  stop("No states were found in the housing_prices table.", call. = FALSE)
}

default_states <- intersect(c("California", "Texas"), state_choices)
if (length(default_states) == 0) {
  default_states <- head(state_choices, 2)
}

money_labels <- function(values) {
  paste0("$", format(round(values), big.mark = ",", scientific = FALSE, trim = TRUE))
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .plot-area {
        margin-bottom: 18px;
      }

      .controls-panel {
        margin-top: 0;
      }

      .controls-panel .form-group {
        margin-bottom: 0;
      }
    "))
  ),
  titlePanel("Interactive Housing Data Dashboard"),

  div(
    class = "plot-area",
    plotOutput("housingPlot", height = "560px")
  ),

  div(
    class = "well controls-panel",
    fluidRow(
      column(
        width = 4,
        selectizeInput("datasets", "States:",
                       choices = state_choices,
                       selected = default_states,
                       multiple = TRUE,
                       width = "100%",
                       options = list(
                         plugins = list("remove_button"),
                         placeholder = "Select states"
                       ))
      ),
      column(
        width = 8,
        sliderInput("dateRange", "Date Range:",
                    min = date_limits$min_date,
                    max = date_limits$max_date,
                    value = c(date_limits$min_date, date_limits$max_date),
                    timeFormat = "%b %Y",
                    width = "100%")
      )
    )
  )
)

# Server
server <- function(input, output) {

  filtered_data <- reactive({
    req(input$datasets)
    req(input$dateRange)
    selected_dates <- as.Date(input$dateRange, origin = "1970-01-01")

    get_state_housing_data(
      states = input$datasets,
      start_date = selected_dates[1],
      end_date = selected_dates[2]
    )
  })

  output$housingPlot <- renderPlot({
    plot_data <- filtered_data()
    validate(need(nrow(plot_data) > 0, "No housing data found for the selected filters."))

    ggplot(plot_data, aes(x = date, y = value, color = dataset)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = money_labels) +
      labs(title = "Housing Values Over Time",
           x = "Date",
           y = "Housing Value",
           color = "State") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_text(size = 11),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(corrplot)
source("db_connect.r")
source("income_query.r")
source("monthly_growth_query.r")

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

  tabsetPanel(
    id = "visualizationTabs",
    tabPanel(
      "Price Trends",
      div(
        class = "plot-area",
        plotOutput("housingPlot", height = "560px")
      )
    ),
    tabPanel(
      "Monthly % Change",
      div(
        class = "plot-area",
        plotOutput("monthlyGrowthPlot", height = "720px")
      )
    )
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

    get_state_income_comparison_data(
      states = input$datasets,
      start_date = selected_dates[1],
      end_date = selected_dates[2]
    )
  })

  monthly_growth_data <- reactive({
    req(input$datasets)
    req(input$dateRange)
    selected_dates <- as.Date(input$dateRange, origin = "1970-01-01")

    get_state_monthly_percent_change_matrix(
      states = input$datasets,
      start_date = selected_dates[1],
      end_date = selected_dates[2]
    )
  })

  output$housingPlot <- renderPlot({
    plot_data <- filtered_data()
    validate(need(nrow(plot_data) > 0, "No housing data found for the selected filters."))

    ggplot(plot_data, aes(x = date, y = value, color = dataset, group = dataset)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = money_labels) +
      labs(title = "Housing Values and Income Over Time",
           x = "Date",
           y = "Dollar Value",
           color = "Series") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.y = element_text(size = 11),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })

  output$monthlyGrowthPlot <- renderPlot({
    monthly_growth_result <- monthly_growth_data()
    percent_change_matrix <- monthly_growth_result$percent_change_matrix

    validate(need(
      !is.null(percent_change_matrix),
      monthly_growth_result$message
    ))

    percent_change_range <- range(percent_change_matrix, na.rm = TRUE)
    if (!all(is.finite(percent_change_range))) {
      percent_change_range <- c(-1, 1)
    } else if (percent_change_range[1] == percent_change_range[2]) {
      percent_change_range <- percent_change_range + c(-1, 1)
    }

    legend_ratio <- min(1, 2 / ncol(percent_change_matrix))

    percent_change_colors <- colorRampPalette(c(
      "#b2182b",
      "#ef8a62",
      "#f7f7f7",
      "#91cf60",
      "#1a9850"
    ))(200)

    suppressWarnings(
      corrplot::corrplot(
        percent_change_matrix,
        method = "color",
        type = "full",
        col = percent_change_colors,
        col.lim = percent_change_range,
        is.corr = FALSE,
        addCoef.col = "#1f2933",
        number.cex = 0.7,
        number.digits = 1,
        tl.pos = "lt",
        tl.col = "#1f2933",
        tl.srt = 35,
        tl.cex = 0.9,
        cl.pos = "r",
        cl.length = 7,
        cl.ratio = legend_ratio,
        cl.cex = 0.9,
        mar = c(0, 0, 2, 0),
        na.label = " "
      )
    )

    title("Average Monthly Housing Price Change (%)", line = 0.5)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

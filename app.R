# Main Shiny application for the housing and income dashboard.
# run_app.R or run_app_3839.R should be used to launch this file.

# Packages required by the app. The check below gives a clearer startup error
# than letting library() fail halfway through app initialization.
required_packages <- c("shiny", "ggplot2", "corrplot", "DBI", "RPostgres", "forecast")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

# Stop early if a required package is missing.
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    ". Install them with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

# Load packages after verifying that all required packages are installed.
library(shiny)
library(ggplot2)
library(corrplot)

# Find the project folder so source() calls work even when RStudio's working
# directory is not the same as the file location.
get_app_directory <- function() {
  source_file <- NULL

  # When app.R is sourced, one of the call frames stores the file path.
  for (frame in sys.frames()) {
    if (!is.null(frame$ofile)) {
      source_file <- frame$ofile
      break
    }
  }

  # If the script is being run interactively from RStudio, use the active file path.
  if ((is.null(source_file) || !nzchar(source_file)) &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    source_file <- rstudioapi::getActiveDocumentContext()$path
  }

  # Use the detected file folder when available; otherwise fall back to getwd().
  if (!is.null(source_file) && nzchar(source_file)) {
    return(dirname(normalizePath(source_file, winslash = "/", mustWork = TRUE)))
  }

  getwd()
}

# Source helper files from the detected app folder.
app_dir <- get_app_directory()
source(file.path(app_dir, "db_connect.r"), chdir = TRUE)
source(file.path(app_dir, "income_query.r"), chdir = TRUE)
source(file.path(app_dir, "monthly_growth_query.r"), chdir = TRUE)
source(file.path(app_dir, "forecast_query.r"), chdir = TRUE)

# Read startup values from the database before the UI is created.
state_choices <- get_state_choices()
date_limits <- get_date_limits()
income_table_sources <- get_income_table_sources()

# The dashboard cannot function if the housing table has no state names.
if (length(state_choices) == 0) {
  stop("No states were found in the housing_prices table.", call. = FALSE)
}

# Prefer common comparison states, but fall back to the first two available states.
default_states <- intersect(c("California", "Texas"), state_choices)
if (length(default_states) == 0) {
  default_states <- head(state_choices, 2)
}

# Format numeric values as dollar amounts for plot axes.
money_labels <- function(values) {
  paste0("$", format(round(values), big.mark = ",", scientific = FALSE, trim = TRUE))
}

# Format affordability ratios for the forecasting tab.
ratio_labels <- function(values) {
  format(round(values, 2), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Return safe slider ranges for the income visualization controls.
get_income_visualization_ranges <- function(plot_data) {
  # Empty data means the control sliders should not be rendered.
  if (nrow(plot_data) == 0) {
    return(list(dates = NULL, values = NULL))
  }

  date_range <- range(plot_data$date, na.rm = TRUE)
  value_range <- range(plot_data$zhvi, na.rm = TRUE)

  # Invalid date ranges are represented as NULL so renderUI can skip that slider.
  if (any(is.na(date_range))) {
    date_range <- NULL
  }

  # Invalid or flat numeric ranges are adjusted so sliderInput has usable bounds.
  if (!all(is.finite(value_range))) {
    value_range <- NULL
  } else if (value_range[1] == value_range[2]) {
    value_range <- value_range + c(-1, 1)
  }

  list(dates = date_range, values = value_range)
}

# High-contrast, colorblind-friendlier colors for the clean and malformed income data.
income_visual_colors <- c(
  "Clean" = "#005AB5",
  "Duplicate Rows" = "#C74700",
  "Improper Normalization" = "#007A5E",
  "Denormalized Mixed Types" = "#8B3A96",
  "Incomplete Data" = "#1F2937",
  "Mutated Values" = "#8A6F00"
)

# Line types and point shapes provide non-color cues for accessibility.
income_visual_line_types <- c(
  "Clean" = "solid",
  "Duplicate Rows" = "dashed",
  "Improper Normalization" = "dotdash",
  "Denormalized Mixed Types" = "twodash",
  "Incomplete Data" = "longdash",
  "Mutated Values" = "dotted"
)

# Point shapes match the same dataset labels used by the colors and line types.
income_visual_shapes <- c(
  "Clean" = 16,
  "Duplicate Rows" = 17,
  "Improper Normalization" = 15,
  "Denormalized Mixed Types" = 18,
  "Incomplete Data" = 4,
  "Mutated Values" = 8
)

# Used by the checkbox controls to enable or disable plotted income datasets.
income_tracked_value_choices <- names(income_visual_colors)

# Forecast colors are separate from income-malformation colors to keep the
# forecast tab focused on model comparison.
forecast_model_colors <- c(
  "Linear Regression" = "#007A5E",
  "ARIMA" = "#005AB5",
  "ETS" = "#C74700"
)

forecast_model_line_types <- c(
  "Linear Regression" = "dotdash",
  "ARIMA" = "solid",
  "ETS" = "dashed"
)

# UI
ui <- fluidPage(
  tags$head(
    # Small layout adjustments for controls, plot spacing, and wide table output.
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

      .controls-panel .btn,
      .income-visual-controls .btn {
        margin-right: 8px;
        margin-top: 8px;
      }

      .income-visual-controls .checkbox-inline {
        margin-right: 12px;
      }

      .income-table-scroll {
        max-height: 560px;
        overflow: auto;
        font-size: 12px;
      }

      .income-table-scroll table {
        white-space: nowrap;
      }

      .forecast-controls .form-group {
        margin-bottom: 0;
      }

      .forecast-controls .btn {
        margin-right: 8px;
        margin-top: 8px;
      }

      .forecast-controls .checkbox-inline {
        margin-right: 12px;
      }
    "))
  ),
  titlePanel("Interactive Housing Data Dashboard"),

  # Main navigation tabs for housing plots, income plots, and raw income tables.
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
    ),
    tabPanel(
      "Affordability Forecasts",
      div(
        class = "well forecast-controls",
        fluidRow(
          column(
            width = 6,
            sliderInput(
              "forecastHorizon",
              "Forecast Horizon:",
              min = 1,
              max = 10,
              value = 5,
              step = 1,
              post = " years",
              width = "100%"
            )
          ),
          column(
            width = 6,
            sliderInput(
              "forecastTestYears",
              "Holdout Test Window:",
              min = 1,
              max = 5,
              value = 3,
              step = 1,
              post = " years",
              width = "100%"
            )
          )
        )
      ),
      div(
        class = "plot-area",
        plotOutput("affordabilityForecastPlot", height = "760px")
      ),
      div(
        class = "income-table-scroll",
        tableOutput("affordabilityForecastMetrics")
      )
    ),
    tabPanel(
      "Income Forecast Impact",
      div(
        class = "well forecast-controls",
        fluidRow(
          column(
            width = 6,
            checkboxGroupInput(
              "incomeForecastDatasets",
              "Income Data Versions:",
              choices = income_tracked_value_choices,
              selected = income_tracked_value_choices,
              inline = TRUE
            )
          ),
          column(
            width = 3,
            sliderInput(
              "incomeForecastHorizon",
              "Forecast Horizon:",
              min = 1,
              max = 10,
              value = 5,
              step = 1,
              post = " years",
              width = "100%"
            )
          ),
          column(
            width = 3,
            sliderInput(
              "incomeForecastTestYears",
              "Holdout Test Window:",
              min = 1,
              max = 5,
              value = 3,
              step = 1,
              post = " years",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            actionButton("enableAllIncomeForecastDatasets", "Enable All", class = "btn-default"),
            actionButton("disableAllIncomeForecastDatasets", "Disable All", class = "btn-default")
          )
        )
      ),
      div(
        class = "plot-area",
        plotOutput("incomeForecastImpactPlot", height = "820px")
      ),
      div(
        class = "income-table-scroll",
        tableOutput("incomeForecastImpactMetrics")
      )
    ),
    tabPanel(
      "Income Data Visuals",
      # Controls are rendered dynamically because their slider ranges come from data.
      uiOutput("incomeVisualControls"),
      div(
        class = "plot-area",
        plotOutput(
          "incomeMalformedLinePlot",
          height = "620px",
          # Brushing lets the user drag over part of the line plot to zoom.
          brush = brushOpts(
            id = "incomeLineBrush",
            direction = "xy",
            resetOnNew = FALSE
          )
        )
      ),
      div(
        class = "plot-area",
        plotOutput("incomeMalformedHistogram", height = "760px")
      )
    ),
    tabPanel(
      "Income Data Tables",
      # Build one table tab for each clean or malformed income source.
      do.call(
        tabsetPanel,
        c(
          list(id = "incomeDataTabs"),
          lapply(seq_len(nrow(income_table_sources)), function(row_index) {
            source_row <- income_table_sources[row_index, ]
            tabPanel(
              source_row$label,
              div(
                class = "income-table-scroll",
                tableOutput(paste0(source_row$key, "Table"))
              )
            )
          })
        )
      )
    )
  ),

  # Shared housing controls used by the price-trend and monthly-change tabs.
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
                       )),
        actionButton("clearStates", "Clear States", class = "btn-default")
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
server <- function(input, output, session) {

  # Clear the state selector without changing the date range.
  observeEvent(input$clearStates, {
    updateSelectizeInput(session, "datasets", selected = character(0))
  })

  # Pull combined housing and income data whenever state or date controls change.
  filtered_data <- reactive({
    req(input$dateRange)
    selected_states <- input$datasets

    # Return an empty data frame so renderPlot can show a friendly validation message.
    if (is.null(selected_states) || length(selected_states) == 0) {
      return(data.frame(
        dataset = character(),
        date = as.Date(character()),
        value = numeric()
      ))
    }

    selected_dates <- as.Date(input$dateRange, origin = "1970-01-01")

    # Query helper handles the SQL filtering and joins.
    get_state_income_comparison_data(
      states = selected_states,
      start_date = selected_dates[1],
      end_date = selected_dates[2]
    )
  })

  # Pull monthly percent-change data for the selected states and date range.
  monthly_growth_data <- reactive({
    req(input$dateRange)
    selected_states <- input$datasets

    # corrplot needs a matrix, so return a message object if no states are selected.
    if (is.null(selected_states) || length(selected_states) == 0) {
      return(list(
        percent_change_matrix = NULL,
        message = "Select at least one state to display monthly changes."
      ))
    }

    selected_dates <- as.Date(input$dateRange, origin = "1970-01-01")

    # Query helper converts long SQL results into the matrix used by corrplot.
    get_state_monthly_percent_change_matrix(
      states = selected_states,
      start_date = selected_dates[1],
      end_date = selected_dates[2]
    )
  })

  # Render the main line chart comparing selected state prices with income data.
  output$housingPlot <- renderPlot({
    plot_data <- filtered_data()
    # validate/need prints the message inside the plot area when there is no data.
    validate(need(nrow(plot_data) > 0, "Select at least one state with data to display the price trends."))

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

  # Render the monthly percent-change heatmap.
  output$monthlyGrowthPlot <- renderPlot({
    monthly_growth_result <- monthly_growth_data()
    percent_change_matrix <- monthly_growth_result$percent_change_matrix

    # Stop rendering and display the helper message if the matrix is unavailable.
    validate(need(
      !is.null(percent_change_matrix),
      monthly_growth_result$message
    ))

    # corrplot needs a usable color scale even when all values are equal or missing.
    percent_change_range <- range(percent_change_matrix, na.rm = TRUE)
    if (!all(is.finite(percent_change_range))) {
      percent_change_range <- c(-1, 1)
    } else if (percent_change_range[1] == percent_change_range[2]) {
      percent_change_range <- percent_change_range + c(-1, 1)
    }

    # Keep the color legend readable as the number of selected states changes.
    legend_ratio <- min(1, 2 / ncol(percent_change_matrix))

    # Diverging palette: red for decreases, white for near-zero, green for increases.
    percent_change_colors <- colorRampPalette(c(
      "#b2182b",
      "#ef8a62",
      "#f7f7f7",
      "#91cf60",
      "#1a9850"
    ))(200)

    # corrplot prints warnings for some NA layouts; suppressWarnings avoids noise
    # while the na.label setting intentionally leaves missing cells blank.
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

  # Fit ARIMA and ETS affordability forecasts for the selected states.
  affordability_forecast_results <- reactive({
    selected_states <- input$datasets

    get_affordability_forecast_results(
      states = selected_states,
      horizon_years = input$forecastHorizon,
      test_years = input$forecastTestYears
    )
  })

  # Render historical affordability ratios plus ARIMA/ETS forecasts.
  output$affordabilityForecastPlot <- renderPlot({
    forecast_results <- affordability_forecast_results()
    history_data <- forecast_results$history
    forecast_data <- forecast_results$forecast
    validation_message <- forecast_results$message

    if (is.null(validation_message)) {
      validation_message <- "Forecast models could not be fit for the selected data."
    }

    validate(need(
      nrow(history_data) > 0 && nrow(forecast_data) > 0,
      validation_message
    ))

    ggplot() +
      geom_line(
        data = history_data,
        aes(x = date, y = value),
        color = "#1F2937",
        linewidth = 0.95
      ) +
      geom_point(
        data = history_data,
        aes(x = date, y = value),
        color = "#1F2937",
        size = 1.8,
        alpha = 0.9
      ) +
      geom_ribbon(
        data = forecast_data,
        aes(x = date, ymin = lower95, ymax = upper95, fill = model),
        alpha = 0.14
      ) +
      geom_ribbon(
        data = forecast_data,
        aes(x = date, ymin = lower80, ymax = upper80, fill = model),
        alpha = 0.24
      ) +
      geom_line(
        data = forecast_data,
        aes(x = date, y = value, color = model, linetype = model),
        linewidth = 1.05
      ) +
      geom_point(
        data = forecast_data,
        aes(x = date, y = value, color = model),
        size = 1.8
      ) +
      facet_wrap(~ dataset, scales = "free_y") +
      scale_color_manual(values = forecast_model_colors) +
      scale_fill_manual(values = forecast_model_colors) +
      scale_linetype_manual(values = forecast_model_line_types) +
      scale_y_continuous(labels = ratio_labels) +
      labs(
        title = "Housing Affordability Forecasts",
        x = "Year",
        y = "Housing Price to Income Ratio",
        color = "Model",
        fill = "Model",
        linetype = "Model"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )
  })

  # Show RMSE and MAE from the holdout comparison for each model.
  output$affordabilityForecastMetrics <- renderTable({
    forecast_results <- affordability_forecast_results()
    metrics <- forecast_results$metrics

    if (nrow(metrics) == 0) {
      validation_message <- forecast_results$message

      if (is.null(validation_message)) {
        validation_message <- "No model performance metrics were available."
      }

      return(data.frame(message = validation_message))
    }

    metrics$rmse <- round(metrics$rmse, 3)
    metrics$mae <- round(metrics$mae, 3)
    metrics[order(metrics$dataset, metrics$rmse, metrics$mae), ]
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "s",
  width = "100%")

  # Enable every income data version in the model-impact tab.
  observeEvent(input$enableAllIncomeForecastDatasets, {
    updateCheckboxGroupInput(
      session,
      "incomeForecastDatasets",
      selected = income_tracked_value_choices
    )
  })

  # Disable every income data version in the model-impact tab.
  observeEvent(input$disableAllIncomeForecastDatasets, {
    updateCheckboxGroupInput(session, "incomeForecastDatasets", selected = character(0))
  })

  # Fit regression, ARIMA, and ETS models to clean and malformed income data.
  income_model_impact_results <- reactive({
    forecast_results <- get_income_model_impact_results(
      horizon_years = input$incomeForecastHorizon,
      test_years = input$incomeForecastTestYears
    )

    selected_datasets <- input$incomeForecastDatasets
    if (is.null(selected_datasets)) {
      selected_datasets <- income_tracked_value_choices
    }

    if (length(selected_datasets) == 0) {
      return(empty_income_model_impact_result(
        "Select at least one income data version to compare model behavior."
      ))
    }

    if (nrow(forecast_results$history) == 0 || nrow(forecast_results$forecast) == 0) {
      return(forecast_results)
    }

    forecast_results$history <- forecast_results$history[
      forecast_results$history$dataset %in% selected_datasets,
    ]
    forecast_results$forecast <- forecast_results$forecast[
      forecast_results$forecast$dataset %in% selected_datasets,
    ]

    if ("dataset" %in% names(forecast_results$metrics)) {
      forecast_results$metrics <- forecast_results$metrics[
        forecast_results$metrics$dataset %in% selected_datasets,
      ]
    }

    if (nrow(forecast_results$history) == 0 || nrow(forecast_results$forecast) == 0) {
      return(empty_income_model_impact_result(
        "No model results were available for the selected income data versions."
      ))
    }

    forecast_results
  })

  # Render how malformed income data changes regression and forecast outputs.
  output$incomeForecastImpactPlot <- renderPlot({
    forecast_results <- income_model_impact_results()
    history_data <- forecast_results$history
    forecast_data <- forecast_results$forecast
    validation_message <- forecast_results$message

    if (is.null(validation_message)) {
      validation_message <- "Income models could not be fit for the selected data."
    }

    validate(need(
      nrow(history_data) > 0 && nrow(forecast_data) > 0,
      validation_message
    ))

    ggplot() +
      geom_line(
        data = history_data,
        aes(x = date, y = value),
        color = "#1F2937",
        linewidth = 0.95
      ) +
      geom_point(
        data = history_data,
        aes(x = date, y = value),
        color = "#1F2937",
        size = 1.8,
        alpha = 0.9
      ) +
      geom_ribbon(
        data = forecast_data,
        aes(x = date, ymin = lower95, ymax = upper95, fill = model),
        alpha = 0.12
      ) +
      geom_ribbon(
        data = forecast_data,
        aes(x = date, ymin = lower80, ymax = upper80, fill = model),
        alpha = 0.22
      ) +
      geom_line(
        data = forecast_data,
        aes(x = date, y = value, color = model, linetype = model),
        linewidth = 1.05
      ) +
      geom_point(
        data = forecast_data,
        aes(x = date, y = value, color = model),
        size = 1.8
      ) +
      facet_wrap(~ dataset, scales = "free_y") +
      scale_color_manual(values = forecast_model_colors) +
      scale_fill_manual(values = forecast_model_colors) +
      scale_linetype_manual(values = forecast_model_line_types) +
      scale_y_continuous(labels = money_labels) +
      labs(
        title = "Income Forecast Impact From Malformed Data",
        x = "Year",
        y = "Displayed Median Income Value",
        color = "Model",
        fill = "Model",
        linetype = "Model"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )
  })

  # Show model accuracy and final forecast movement for each income data version.
  output$incomeForecastImpactMetrics <- renderTable({
    forecast_results <- income_model_impact_results()
    metrics <- forecast_results$metrics

    if (nrow(metrics) == 0 || !("dataset" %in% names(metrics))) {
      validation_message <- forecast_results$message

      if (is.null(validation_message)) {
        validation_message <- "No income model performance metrics were available."
      }

      return(data.frame(message = validation_message))
    }

    history_data <- forecast_results$history[
      order(forecast_results$history$dataset, forecast_results$history$observation_year),
    ]
    latest_history <- do.call(rbind, lapply(split(history_data, history_data$dataset), tail, 1))
    latest_history <- data.frame(
      dataset = latest_history$dataset,
      last_historical_value = latest_history$value,
      stringsAsFactors = FALSE
    )

    forecast_data <- forecast_results$forecast[
      order(
        forecast_results$forecast$dataset,
        forecast_results$forecast$model,
        forecast_results$forecast$observation_year
      ),
    ]
    latest_forecast <- do.call(
      rbind,
      lapply(split(forecast_data, paste(forecast_data$dataset, forecast_data$model)), tail, 1)
    )
    latest_forecast <- data.frame(
      dataset = latest_forecast$dataset,
      model = latest_forecast$model,
      final_forecast_value = latest_forecast$value,
      stringsAsFactors = FALSE
    )

    metrics <- merge(metrics, latest_history, by = "dataset", all.x = TRUE)
    metrics <- merge(metrics, latest_forecast, by = c("dataset", "model"), all.x = TRUE)
    metrics$forecast_change_percent <- 100 *
      (metrics$final_forecast_value - metrics$last_historical_value) /
      metrics$last_historical_value

    metrics$rmse <- round(metrics$rmse, 2)
    metrics$mae <- round(metrics$mae, 2)
    metrics$last_historical_value <- round(metrics$last_historical_value, 2)
    metrics$final_forecast_value <- round(metrics$final_forecast_value, 2)
    metrics$forecast_change_percent <- round(metrics$forecast_change_percent, 2)
    metrics[order(metrics$dataset, metrics$rmse, metrics$mae), ]
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  spacing = "s",
  width = "100%")

  # Pull all available clean/malformed income plot data from the database.
  income_visualization_data <- reactive({
    get_income_visualization_data()
  })

  # Apply the income dataset checkboxes and zoom sliders before plotting.
  selected_income_visualization_data <- reactive({
    plot_data <- income_visualization_data()
    selected_values <- input$incomeTrackedValues

    # Before the dynamic UI finishes rendering, default to showing all datasets.
    if (is.null(selected_values)) {
      selected_values <- income_tracked_value_choices
    }

    # Keep only the datasets the user has enabled.
    plot_data <- plot_data[plot_data$dataset %in% selected_values, ]

    # Apply date zoom when the date slider exists.
    if (!is.null(input$incomeDateZoom) && length(input$incomeDateZoom) == 2) {
      selected_dates <- as.Date(input$incomeDateZoom, origin = "1970-01-01")
      plot_data <- plot_data[
        plot_data$date >= selected_dates[1] & plot_data$date <= selected_dates[2],
      ]
    }

    # Apply value zoom when the value slider exists.
    if (!is.null(input$incomeValueZoom) && length(input$incomeValueZoom) == 2) {
      selected_values <- as.numeric(input$incomeValueZoom)
      plot_data <- plot_data[
        plot_data$zhvi >= selected_values[1] & plot_data$zhvi <= selected_values[2],
      ]
    }

    plot_data
  })

  # Render income-specific controls after data has been read so slider ranges are known.
  output$incomeVisualControls <- renderUI({
    plot_data <- income_visualization_data()
    ranges <- get_income_visualization_ranges(plot_data)

    tagList(
      div(
        class = "well income-visual-controls",
        fluidRow(
          column(
            width = 6,
            checkboxGroupInput(
              "incomeTrackedValues",
              "Tracked Income Values:",
              choices = income_tracked_value_choices,
              selected = income_tracked_value_choices,
              inline = TRUE
            )
          ),
          column(
            width = 3,
            # Only show the date slider when the database returned valid dates.
            if (!is.null(ranges$dates)) {
              sliderInput(
                "incomeDateZoom",
                "Income Date Zoom:",
                min = ranges$dates[1],
                max = ranges$dates[2],
                value = ranges$dates,
                timeFormat = "%Y",
                width = "100%"
              )
            }
          ),
          column(
            width = 3,
            # Only show the value slider when the database returned valid values.
            if (!is.null(ranges$values)) {
              sliderInput(
                "incomeValueZoom",
                "Income Value Zoom:",
                min = floor(ranges$values[1]),
                max = ceiling(ranges$values[2]),
                value = ranges$values,
                pre = "$",
                sep = ",",
                width = "100%"
              )
            }
          )
        ),
        fluidRow(
          column(
            width = 12,
            actionButton("enableAllIncomeValues", "Enable All", class = "btn-default"),
            actionButton("disableAllIncomeValues", "Disable All", class = "btn-default"),
            actionButton("resetIncomeZoom", "Reset Zoom", class = "btn-default")
          )
        )
      )
    )
  })

  # Enable every income dataset checkbox.
  observeEvent(input$enableAllIncomeValues, {
    updateCheckboxGroupInput(
      session,
      "incomeTrackedValues",
      selected = income_tracked_value_choices
    )
  })

  # Disable every income dataset checkbox.
  observeEvent(input$disableAllIncomeValues, {
    updateCheckboxGroupInput(session, "incomeTrackedValues", selected = character(0))
  })

  # Reset the income zoom sliders to the full available data ranges.
  observeEvent(input$resetIncomeZoom, {
    ranges <- get_income_visualization_ranges(income_visualization_data())

    # Update each slider only when it was rendered by renderUI.
    if (!is.null(ranges$dates)) {
      updateSliderInput(session, "incomeDateZoom", value = ranges$dates)
    }

    if (!is.null(ranges$values)) {
      updateSliderInput(session, "incomeValueZoom", value = ranges$values)
    }
  })

  # Convert a mouse brush on the income line plot into date and value zoom ranges.
  observeEvent(input$incomeLineBrush, {
    brush <- input$incomeLineBrush

    # Ignore the observer when there is no active brush selection.
    if (is.null(brush)) {
      return()
    }

    # Brush x-values are numeric dates, so convert them back to Date objects.
    if (!is.null(input$incomeDateZoom)) {
      brush_dates <- sort(as.Date(c(brush$xmin, brush$xmax), origin = "1970-01-01"))
      if (all(!is.na(brush_dates))) {
        updateSliderInput(session, "incomeDateZoom", value = brush_dates)
      }
    }

    # Brush y-values map directly to the income value slider.
    if (!is.null(input$incomeValueZoom)) {
      brush_values <- sort(c(brush$ymin, brush$ymax))
      if (all(is.finite(brush_values))) {
        updateSliderInput(session, "incomeValueZoom", value = brush_values)
      }
    }
  })

  # Render the clean vs malformed income line chart.
  output$incomeMalformedLinePlot <- renderPlot({
    plot_data <- selected_income_visualization_data()
    # Show a message when filters remove all income data.
    validate(need(
      nrow(plot_data) > 0,
      "No income data found for the selected tracked values and zoom range."
    ))

    ggplot(plot_data, aes(
      x = date,
      y = zhvi,
      color = dataset,
      linetype = dataset,
      shape = dataset,
      group = dataset
    )) +
      geom_line(linewidth = 1.05, alpha = 0.95) +
      geom_point(size = 1.9, alpha = 0.9) +
      scale_color_manual(values = income_visual_colors) +
      scale_linetype_manual(values = income_visual_line_types) +
      scale_shape_manual(values = income_visual_shapes) +
      scale_y_continuous(labels = money_labels) +
      labs(
        title = "Clean vs Malformed Median Income Displays",
        x = "Date",
        y = "Displayed Value",
        color = "Dataset",
        linetype = "Dataset",
        shape = "Dataset"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })

  # Render the income-value distribution histogram.
  output$incomeMalformedHistogram <- renderPlot({
    plot_data <- selected_income_visualization_data()
    # Show a message when filters remove all histogram data.
    validate(need(
      nrow(plot_data) > 0,
      "No income data found for the selected tracked values and zoom range."
    ))

    ggplot(plot_data, aes(x = zhvi, fill = dataset)) +
      geom_histogram(bins = 12, alpha = 0.92, color = "#F8FAFC", linewidth = 0.35) +
      scale_fill_manual(values = income_visual_colors) +
      scale_x_continuous(labels = money_labels) +
      facet_wrap(~ dataset, scales = "free_x") +
      labs(
        title = "Distribution of Displayed Median Income Values",
        x = "Displayed Value",
        y = "Count"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold")
      )
  })

  # Create one renderTable output for each income data source.
  for (row_index in seq_len(nrow(income_table_sources))) {
    local({
      # local() captures the current loop values so every tab uses its own key.
      source_key <- income_table_sources$key[row_index]
      output_id <- paste0(source_key, "Table")

      # get_income_table_data() handles missing or broken database views gracefully.
      output[[output_id]] <- renderTable(
        get_income_table_data(source_key),
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        spacing = "s",
        width = "100%"
      )
    })
  }
}

# Run the app
shinyApp(ui = ui, server = server)

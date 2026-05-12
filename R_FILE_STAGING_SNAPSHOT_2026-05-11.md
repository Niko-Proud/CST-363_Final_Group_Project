# R File Staging Snapshot

Created before pulling remote changes so the current R code can be recovered or compared if needed.

## app.R

```r
# Main Shiny application for the housing and income dashboard.
# run_app.R or run_app_3839.R should be used to launch this file.

# Packages required by the app. The check below gives a clearer startup error
# than letting library() fail halfway through app initialization.
required_packages <- c("shiny", "ggplot2", "corrplot", "DBI", "RPostgres")
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

```

## db_connect.r

```r
# Database connection and shared housing-data query helpers.
# These functions are sourced by app.R and should not launch the Shiny app.

library(DBI)
library(RPostgres)

# Open one PostgreSQL connection using the project database settings.
# Callers are responsible for disconnecting the returned connection.
get_db_connection <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = "final_project_housing_analysis",
    host = "localhost",
    port = 5431,
    user = "postgres",
    password = "ott3r"
  )
}

# Return one average housing value per date across the whole housing table.
# This helper is useful for simple plots or quick database checks.
get_plot_data <- function() {
  con <- get_db_connection()
  # Always close the database connection when this function exits.
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(con, "
    SELECT
      date::date AS date,
      AVG(COALESCE(zhvi, price)) AS zhvi
    FROM housing_prices
    WHERE date IS NOT NULL
      AND COALESCE(zhvi, price) IS NOT NULL
    GROUP BY date
    ORDER BY date;
  ")

  result$date <- as.Date(result$date)
  result$zhvi <- as.numeric(result$zhvi)
  result
}

# Return the list of states available in the housing_prices table.
# The Shiny UI uses this to populate the state selector.
get_state_choices <- function() {
  con <- get_db_connection()
  # Prevent open connections from piling up after repeated app refreshes.
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(con, "
    SELECT DISTINCT state
    FROM housing_prices
    WHERE state IS NOT NULL
    ORDER BY state;
  ")

  result$state
}

# Return the earliest and latest usable housing dates.
# The Shiny UI uses these values to set the date-range slider.
get_date_limits <- function() {
  con <- get_db_connection()
  # Close the connection even when the validation below stops with an error.
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(con, "
    SELECT
      MIN(date)::date AS min_date,
      MAX(date)::date AS max_date
    FROM housing_prices
    WHERE date IS NOT NULL
      AND COALESCE(zhvi, price) IS NOT NULL;
  ")

  # Stop app startup early if the housing table exists but has no usable dates.
  if (nrow(result) == 0 || is.na(result$min_date[1]) || is.na(result$max_date[1])) {
    stop("No valid housing price dates were found in the database.", call. = FALSE)
  }

  data.frame(
    min_date = as.Date(result$min_date[1]),
    max_date = as.Date(result$max_date[1])
  )
}

# Return housing values filtered by optional states and date bounds.
# Dynamic WHERE clauses let the same function support the main line chart controls.
get_state_housing_data <- function(states = NULL, start_date = NULL, end_date = NULL) {
  con <- get_db_connection()
  # dbDisconnect runs after the query finishes, even if an error occurs.
  on.exit(dbDisconnect(con), add = TRUE)

  # Base filters remove rows that cannot be plotted.
  filters <- c(
    "state IS NOT NULL",
    "date IS NOT NULL",
    "COALESCE(zhvi, price) IS NOT NULL"
  )

  # Add a state filter only when the user has selected one or more states.
  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    filters <- c(filters, paste0("state IN (", quoted_states, ")"))
  }

  # Add lower and upper date limits only when those controls are available.
  if (!is.null(start_date)) {
    filters <- c(filters, paste("date >=", dbQuoteLiteral(con, as.Date(start_date))))
  }

  if (!is.null(end_date)) {
    filters <- c(filters, paste("date <=", dbQuoteLiteral(con, as.Date(end_date))))
  }

  # Build the SQL from the validated filter list.
  query <- paste(
    "SELECT",
    "  state AS dataset,",
    "  date::date AS date,",
    "  AVG(COALESCE(zhvi, price)) AS value",
    "FROM housing_prices",
    "WHERE", paste(filters, collapse = " AND "),
    "GROUP BY state, date",
    "ORDER BY state, date;"
  )

  result <- dbGetQuery(con, query)
  result$date <- as.Date(result$date)
  result$value <- as.numeric(result$value)
  result
}

```

## income_query.r

```r
# Income and housing comparison query helpers.
# app.R sources this file after db_connect.r so get_db_connection() is available.

# Return state housing prices, weighted average housing prices, and national
# median household income in one long-format data set for the main comparison plot.
get_state_income_comparison_data <- function(states = NULL, start_date = NULL, end_date = NULL) {
  con <- get_db_connection()
  # Close this connection when the query helper finishes.
  on.exit(dbDisconnect(con), add = TRUE)

  # Base housing filters keep only rows that can be plotted.
  housing_filters <- c(
    "state IS NOT NULL",
    "date IS NOT NULL",
    "COALESCE(zhvi, price) IS NOT NULL"
  )

  # Weighted housing uses the same base filters, but should not be restricted
  # by selected states because it represents the overall weighted average.
  weighted_housing_filters <- housing_filters

  # The comparison plot uses the median household income series.
  income_filters <- c(
    "s.series_code = 'MEHOINUSA646N'",
    "d.observation_date IS NOT NULL",
    "io.observation_value IS NOT NULL"
  )

  # Restrict state housing lines to the selected states, if any are selected.
  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    housing_filters <- c(housing_filters, paste0("state IN (", quoted_states, ")"))
  }

  # Apply the date slider to the state housing, weighted housing, and income rows.
  if (!is.null(start_date)) {
    start_date_literal <- dbQuoteLiteral(con, as.Date(start_date))
    housing_filters <- c(housing_filters, paste("date >=", start_date_literal))
    weighted_housing_filters <- c(weighted_housing_filters, paste("date >=", start_date_literal))
    income_filters <- c(income_filters, paste("d.observation_date >=", start_date_literal))
  }

  if (!is.null(end_date)) {
    end_date_literal <- dbQuoteLiteral(con, as.Date(end_date))
    housing_filters <- c(housing_filters, paste("date <=", end_date_literal))
    weighted_housing_filters <- c(weighted_housing_filters, paste("date <=", end_date_literal))
    income_filters <- c(income_filters, paste("d.observation_date <=", end_date_literal))
  }

  # UNION ALL combines the selected state lines, weighted housing line,
  # and income line into the same columns for ggplot.
  query <- paste(
    "SELECT dataset, date, value",
    "FROM (",
    "  SELECT",
    "    state AS dataset,",
    "    date::date AS date,",
    "    AVG(COALESCE(zhvi, price)) AS value",
    "  FROM housing_prices",
    "  WHERE", paste(housing_filters, collapse = " AND "),
    "  GROUP BY state, date",
    "",
    "  UNION ALL",
    "",
    "  SELECT",
    "    'Weighted Average Housing Price' AS dataset,",
    "    state_prices.date AS date,",
    "    SUM(state_prices.value * u.revised_housing_units::numeric) /",
    "      NULLIF(SUM(u.revised_housing_units::numeric), 0) AS value",
    "  FROM (",
    "    SELECT",
    "      state,",
    "      date::date AS date,",
    "      AVG(COALESCE(zhvi, price)) AS value",
    "    FROM housing_prices",
    "    WHERE", paste(weighted_housing_filters, collapse = " AND "),
    "    GROUP BY state, date",
    "  ) state_prices",
    "  JOIN public.total_housing_units_by_state u",
    "    ON u.state_or_territory = state_prices.state",
    "  WHERE u.revised_housing_units IS NOT NULL",
    "    AND u.revised_housing_units > 0",
    "  GROUP BY state_prices.date",
    "",
    "  UNION ALL",
    "",
    "  SELECT",
    "    CONCAT('Income: ', s.series_name) AS dataset,",
    "    d.observation_date::date AS date,",
    "    io.observation_value AS value",
    "  FROM income.income_observation io",
    "  JOIN income.income_series s",
    "    ON s.series_id = io.series_id",
    "  JOIN income.observation_date d",
    "    ON d.date_id = io.date_id",
    "  WHERE", paste(income_filters, collapse = " AND "),
    ") comparison_data",
    "ORDER BY dataset, date;"
  )

  result <- dbGetQuery(con, query)
  result$date <- as.Date(result$date)
  result$value <- as.numeric(result$value)
  result
}

# Define the table tabs used by app.R for clean and malformed income data.
# Each row maps a UI key and label to one database view.
get_income_table_sources <- function() {
  data.frame(
    key = c(
      "clean",
      "duplicate_rows",
      "improperly_normalized",
      "denormalized_mixed_types",
      "incomplete_data",
      "mutated_values"
    ),
    label = c(
      "Clean Income Data",
      "Malformed: Duplicate Rows",
      "Malformed: Improper Normalization",
      "Malformed: Denormalized Mixed Types",
      "Malformed: Incomplete Data",
      "Malformed: Mutated Values"
    ),
    view_name = c(
      "income.v_income_observations_long",
      "income_bad_duplicates.v_observations_long",
      "income_bad_normalization.v_observations_long",
      "income_bad_heavy.v_observations_long",
      "income_bad_incomplete.v_observations_long",
      "income_bad_mutated.v_observations_long"
    ),
    stringsAsFactors = FALSE
  )
}

# Return the contents of one income table view for the table tab UI.
# Instead of crashing when a malformed schema is missing, this returns a message table.
get_income_table_data <- function(source_key) {
  sources <- get_income_table_sources()
  source_row <- sources[sources$key == source_key, ]

  # Guard against typos or stale UI keys.
  if (nrow(source_row) != 1) {
    return(data.frame(message = paste("Unknown income table source:", source_key)))
  }

  con <- get_db_connection()
  # Disconnect after the existence check and table query.
  on.exit(dbDisconnect(con), add = TRUE)

  view_name <- source_row$view_name[1]
  # to_regclass checks whether a view/table exists without raising an error.
  view_exists_query <- paste(
    "SELECT to_regclass(",
    dbQuoteString(con, view_name),
    ") IS NOT NULL AS view_exists;"
  )
  view_exists <- dbGetQuery(con, view_exists_query)$view_exists[1]

  # Missing malformed views are shown as readable messages in the Shiny table.
  if (!isTRUE(view_exists)) {
    return(data.frame(
      message = paste(
        "Database view not found:",
        view_name,
        "- run the matching SQL file before opening this table."
      )
    ))
  }

  query <- paste(
    "SELECT *",
    "FROM", view_name,
    "ORDER BY observation_date NULLS LAST, series_code NULLS LAST;"
  )

  # tryCatch keeps the app running if a view exists but has a broken definition.
  tryCatch(
    dbGetQuery(con, query),
    error = function(error) {
      data.frame(message = conditionMessage(error))
    }
  )
}

# Define the clean and malformed views used by the income line and histogram plots.
get_income_plot_sources <- function() {
  data.frame(
    label = c(
      "Clean",
      "Duplicate Rows",
      "Improper Normalization",
      "Denormalized Mixed Types",
      "Incomplete Data",
      "Mutated Values"
    ),
    view_name = c(
      "income.v_income_observations_long",
      "income_bad_duplicates.v_plot_data",
      "income_bad_normalization.v_plot_data",
      "income_bad_heavy.v_plot_data",
      "income_bad_incomplete.v_plot_data",
      "income_bad_mutated.v_plot_data"
    ),
    stringsAsFactors = FALSE
  )
}

# Read all available income plot views and stack them into one long-format data frame.
get_income_visualization_data <- function() {
  con <- get_db_connection()
  # Ensure repeated Shiny refreshes do not leave idle database connections behind.
  on.exit(dbDisconnect(con), add = TRUE)

  sources <- get_income_plot_sources()
  plot_data <- data.frame()

  # Loop through each possible clean/malformed plot source and include only
  # the views that actually exist in the current database.
  for (row_index in seq_len(nrow(sources))) {
    source_row <- sources[row_index, ]
    view_exists_query <- paste(
      "SELECT to_regclass(",
      dbQuoteString(con, source_row$view_name),
      ") IS NOT NULL AS view_exists;"
    )
    view_exists <- dbGetQuery(con, view_exists_query)$view_exists[1]

    # Skip missing malformed schemas so one missing SQL file does not break the app.
    if (!isTRUE(view_exists)) {
      next
    }

    # The clean schema uses the long observation view; malformed schemas expose
    # simpler v_plot_data views with date and zhvi columns.
    query <- if (source_row$view_name == "income.v_income_observations_long") {
      paste(
        "SELECT",
        "  observation_date::date AS date,",
        "  observation_value AS zhvi",
        "FROM income.v_income_observations_long",
        "WHERE series_code = 'MEHOINUSA646N'",
        "ORDER BY observation_date;"
      )
    } else {
      paste(
        "SELECT",
        "  date::date AS date,",
        "  zhvi",
        "FROM", source_row$view_name,
        "ORDER BY date;"
      )
    }

    # If a malformed view has an error, ignore just that source and keep the rest.
    source_data <- tryCatch(
      dbGetQuery(con, query),
      error = function(error) data.frame()
    )

    # Empty sources do not contribute a plotted line or histogram facet.
    if (nrow(source_data) == 0) {
      next
    }

    # Normalize column types and label the rows with the display name.
    source_data$date <- as.Date(source_data$date)
    source_data$zhvi <- as.numeric(source_data$zhvi)
    source_data$dataset <- source_row$label
    plot_data <- rbind(plot_data, source_data)
  }

  plot_data
}

```

## monthly_growth_query.r

```r
# Monthly housing percent-change query helpers.
# app.R uses these functions to build the monthly heatmap/correlation-style plot.

# Return average percent change by state and month number for the selected filters.
get_state_monthly_percent_change_data <- function(states = NULL, start_date = NULL, end_date = NULL) {
  con <- get_db_connection()
  # Close the database connection after the query finishes.
  on.exit(dbDisconnect(con), add = TRUE)

  # Base filters remove rows that cannot be averaged into monthly prices.
  filters <- c(
    "state IS NOT NULL",
    "date IS NOT NULL",
    "COALESCE(zhvi, price) IS NOT NULL"
  )

  # Output filters are applied after monthly percent changes are calculated.
  output_filters <- c("monthly_percent_change IS NOT NULL")

  # Limit the query to selected states when the user has chosen any.
  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    filters <- c(filters, paste0("state IN (", quoted_states, ")"))
  }

  # Include the prior month in the raw data so LAG() can calculate the first
  # visible month's percent change accurately.
  if (!is.null(start_date)) {
    start_month <- as.Date(format(as.Date(start_date), "%Y-%m-01"))
    start_month_literal <- dbQuoteLiteral(con, start_month)
    filters <- c(filters, paste("date >=", start_month_literal, "- INTERVAL '1 month'"))
    output_filters <- c(output_filters, paste("month_date >=", start_month_literal))
  }

  # Cap the raw rows and final output at the selected ending month.
  if (!is.null(end_date)) {
    end_month <- as.Date(format(as.Date(end_date), "%Y-%m-01"))
    end_month_literal <- dbQuoteLiteral(con, end_month)
    filters <- c(filters, paste("date <", end_month_literal, "+ INTERVAL '1 month'"))
    output_filters <- c(output_filters, paste("month_date <=", end_month_literal))
  }

  # monthly_prices groups daily/row-level prices into months; monthly_changes
  # compares each month against the previous month for the same state.
  query <- paste(
    "WITH monthly_prices AS (",
    "  SELECT",
    "    state,",
    "    DATE_TRUNC('month', date)::date AS month_date,",
    "    AVG(COALESCE(zhvi, price)) AS monthly_price",
    "  FROM housing_prices",
    "  WHERE", paste(filters, collapse = " AND "),
    "  GROUP BY state, DATE_TRUNC('month', date)::date",
    "), monthly_changes AS (",
    "  SELECT",
    "    state,",
    "    month_date,",
    "    EXTRACT(MONTH FROM month_date)::integer AS observation_month,",
    "    100 * (monthly_price - LAG(monthly_price) OVER (PARTITION BY state ORDER BY month_date)) /",
    "      NULLIF(LAG(monthly_price) OVER (PARTITION BY state ORDER BY month_date), 0) AS monthly_percent_change",
    "  FROM monthly_prices",
    ")",
    "SELECT",
    "  state,",
    "  observation_month,",
    "  AVG(monthly_percent_change) AS monthly_percent_change",
    "FROM monthly_changes",
    "WHERE", paste(output_filters, collapse = " AND "),
    "GROUP BY state, observation_month",
    "ORDER BY observation_month, state;"
  )

  result <- dbGetQuery(con, query)
  result$observation_month <- as.integer(result$observation_month)
  result$monthly_percent_change <- as.numeric(result$monthly_percent_change)
  result
}

# Convert the long monthly percent-change data into a 12-row matrix.
# corrplot expects matrix-like input, so app.R uses this shape for display.
get_state_monthly_percent_change_matrix <- function(states = NULL, start_date = NULL, end_date = NULL) {
  monthly_data <- get_state_monthly_percent_change_data(
    states = states,
    start_date = start_date,
    end_date = end_date
  )

  # Tell the UI why there is nothing to render instead of returning an empty plot.
  if (nrow(monthly_data) == 0) {
    return(list(
      percent_change_matrix = NULL,
      monthly_data = monthly_data,
      message = "No monthly housing price percent-change data found for the selected filters."
    ))
  }

  # Keep selected states in selector order; otherwise sort whatever came back.
  selected_states <- if (!is.null(states) && length(states) > 0) {
    states
  } else {
    sort(unique(monthly_data$state))
  }

  # Pre-fill all month/state combinations with NA so missing months remain visible.
  percent_change_matrix <- matrix(
    NA_real_,
    nrow = 12,
    ncol = length(selected_states),
    dimnames = list(month.abb, selected_states)
  )

  # Fill the matrix one database result row at a time.
  for (row_index in seq_len(nrow(monthly_data))) {
    month_number <- monthly_data$observation_month[row_index]
    state <- monthly_data$state[row_index]

    # Only write valid month numbers for states still present in the selected set.
    if (!is.na(month_number) && state %in% selected_states) {
      percent_change_matrix[month.abb[month_number], state] <-
        monthly_data$monthly_percent_change[row_index]
    }
  }

  # Drop states with no calculated percent changes after filtering.
  populated_states <- colSums(!is.na(percent_change_matrix)) > 0
  percent_change_matrix <- percent_change_matrix[, populated_states, drop = FALSE]

  # Return a readable message if every selected state was dropped.
  if (ncol(percent_change_matrix) == 0) {
    return(list(
      percent_change_matrix = NULL,
      monthly_data = monthly_data,
      message = "No selected states had enough monthly values to calculate percent change."
    ))
  }

  list(
    percent_change_matrix = percent_change_matrix,
    monthly_data = monthly_data,
    message = NULL
  )
}

```

## run_app.R

```r
# Primary Shiny launcher for the project.
# Source this file from RStudio to start the app on http://127.0.0.1:3838.

# Locate the folder containing this launcher so runApp() does not depend on
# whatever working directory RStudio currently has selected.
get_launcher_directory <- function() {
  source_files <- vapply(sys.frames(), function(frame) {
    # frame$ofile is set when this script is run through source().
    if (is.null(frame$ofile)) {
      NA_character_
    } else {
      frame$ofile
    }
  }, character(1))

  source_files <- source_files[!is.na(source_files)]

  # Prefer the sourced file path because it points at the project folder.
  if (length(source_files) > 0) {
    return(dirname(normalizePath(tail(source_files, 1), winslash = "/", mustWork = TRUE)))
  }

  # Fall back to the working directory when the script is run another way.
  working_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  # Accept the working directory only if it actually contains app.R.
  if (file.exists(file.path(working_directory, "app.R"))) {
    return(working_directory)
  }

  # Stop with a clear message instead of letting Shiny throw a vague app-dir error.
  stop("Could not find app.R. Open the project folder or source this launcher file directly.")
}

app_directory <- get_launcher_directory()

# Final guard before launching the app.
if (!file.exists(file.path(app_directory, "app.R"))) {
  stop("The launcher directory does not contain app.R: ", app_directory)
}

# Start the app and ask Shiny/RStudio to open the browser.
shiny::runApp(app_directory, host = "127.0.0.1", port = 3838, launch.browser = TRUE)

```

## run_app_3839.R

```r
# Alternate Shiny launcher for the project.
# Use this file when port 3838 is busy; it starts on http://127.0.0.1:3839.

# Locate the folder containing this launcher so runApp() does not depend on
# whatever working directory RStudio currently has selected.
get_launcher_directory <- function() {
  source_files <- vapply(sys.frames(), function(frame) {
    # frame$ofile is set when this script is run through source().
    if (is.null(frame$ofile)) {
      NA_character_
    } else {
      frame$ofile
    }
  }, character(1))

  source_files <- source_files[!is.na(source_files)]

  # Prefer the sourced file path because it points at the project folder.
  if (length(source_files) > 0) {
    return(dirname(normalizePath(tail(source_files, 1), winslash = "/", mustWork = TRUE)))
  }

  # Fall back to the working directory when the script is run another way.
  working_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  # Accept the working directory only if it actually contains app.R.
  if (file.exists(file.path(working_directory, "app.R"))) {
    return(working_directory)
  }

  # Stop with a clear message instead of letting Shiny throw a vague app-dir error.
  stop("Could not find app.R. Open the project folder or source this launcher file directly.")
}

app_directory <- get_launcher_directory()

# Final guard before launching the app.
if (!file.exists(file.path(app_directory, "app.R"))) {
  stop("The launcher directory does not contain app.R: ", app_directory)
}

# Start the app and ask Shiny/RStudio to open the browser.
shiny::runApp(app_directory, host = "127.0.0.1", port = 3839, launch.browser = TRUE)

```


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

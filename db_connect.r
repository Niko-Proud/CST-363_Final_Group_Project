library(DBI)
library(RPostgres)

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

get_plot_data <- function() {
  con <- get_db_connection()
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

get_state_choices <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(con, "
    SELECT DISTINCT state
    FROM housing_prices
    WHERE state IS NOT NULL
    ORDER BY state;
  ")

  result$state
}

get_date_limits <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  result <- dbGetQuery(con, "
    SELECT
      MIN(date)::date AS min_date,
      MAX(date)::date AS max_date
    FROM housing_prices
    WHERE date IS NOT NULL
      AND COALESCE(zhvi, price) IS NOT NULL;
  ")

  if (nrow(result) == 0 || is.na(result$min_date[1]) || is.na(result$max_date[1])) {
    stop("No valid housing price dates were found in the database.", call. = FALSE)
  }

  data.frame(
    min_date = as.Date(result$min_date[1]),
    max_date = as.Date(result$max_date[1])
  )
}

get_state_housing_data <- function(states = NULL, start_date = NULL, end_date = NULL) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  filters <- c(
    "state IS NOT NULL",
    "date IS NOT NULL",
    "COALESCE(zhvi, price) IS NOT NULL"
  )

  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    filters <- c(filters, paste0("state IN (", quoted_states, ")"))
  }

  if (!is.null(start_date)) {
    filters <- c(filters, paste("date >=", dbQuoteLiteral(con, as.Date(start_date))))
  }

  if (!is.null(end_date)) {
    filters <- c(filters, paste("date <=", dbQuoteLiteral(con, as.Date(end_date))))
  }

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

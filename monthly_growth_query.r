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

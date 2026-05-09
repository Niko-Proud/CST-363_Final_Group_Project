get_state_monthly_percent_change_data <- function(states = NULL, start_date = NULL, end_date = NULL) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  filters <- c(
    "state IS NOT NULL",
    "date IS NOT NULL",
    "COALESCE(zhvi, price) IS NOT NULL"
  )

  output_filters <- c("monthly_percent_change IS NOT NULL")

  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    filters <- c(filters, paste0("state IN (", quoted_states, ")"))
  }

  if (!is.null(start_date)) {
    start_month <- as.Date(format(as.Date(start_date), "%Y-%m-01"))
    start_month_literal <- dbQuoteLiteral(con, start_month)
    filters <- c(filters, paste("date >=", start_month_literal, "- INTERVAL '1 month'"))
    output_filters <- c(output_filters, paste("month_date >=", start_month_literal))
  }

  if (!is.null(end_date)) {
    end_month <- as.Date(format(as.Date(end_date), "%Y-%m-01"))
    end_month_literal <- dbQuoteLiteral(con, end_month)
    filters <- c(filters, paste("date <", end_month_literal, "+ INTERVAL '1 month'"))
    output_filters <- c(output_filters, paste("month_date <=", end_month_literal))
  }

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

get_state_monthly_percent_change_matrix <- function(states = NULL, start_date = NULL, end_date = NULL) {
  monthly_data <- get_state_monthly_percent_change_data(
    states = states,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(monthly_data) == 0) {
    return(list(
      percent_change_matrix = NULL,
      monthly_data = monthly_data,
      message = "No monthly housing price percent-change data found for the selected filters."
    ))
  }

  selected_states <- if (!is.null(states) && length(states) > 0) {
    states
  } else {
    sort(unique(monthly_data$state))
  }

  percent_change_matrix <- matrix(
    NA_real_,
    nrow = 12,
    ncol = length(selected_states),
    dimnames = list(month.abb, selected_states)
  )

  for (row_index in seq_len(nrow(monthly_data))) {
    month_number <- monthly_data$observation_month[row_index]
    state <- monthly_data$state[row_index]

    if (!is.na(month_number) && state %in% selected_states) {
      percent_change_matrix[month.abb[month_number], state] <-
        monthly_data$monthly_percent_change[row_index]
    }
  }

  populated_states <- colSums(!is.na(percent_change_matrix)) > 0
  percent_change_matrix <- percent_change_matrix[, populated_states, drop = FALSE]

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

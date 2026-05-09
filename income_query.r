get_state_income_comparison_data <- function(states = NULL, start_date = NULL, end_date = NULL) {
  con <- get_db_connection()
  on.exit(dbDisconnect(con), add = TRUE)

  housing_filters <- c(
    "state IS NOT NULL",
    "date IS NOT NULL",
    "COALESCE(zhvi, price) IS NOT NULL"
  )

  weighted_housing_filters <- housing_filters

  income_filters <- c(
    "s.series_code = 'MEHOINUSA646N'",
    "d.observation_date IS NOT NULL",
    "io.observation_value IS NOT NULL"
  )

  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    housing_filters <- c(housing_filters, paste0("state IN (", quoted_states, ")"))
  }

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

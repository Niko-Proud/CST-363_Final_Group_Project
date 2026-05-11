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

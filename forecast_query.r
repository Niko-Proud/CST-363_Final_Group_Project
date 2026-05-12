# Forecasting helpers for housing affordability.
# app.R sources this file after db_connect.r so get_db_connection() is available.

# Return annual housing affordability history for selected states plus the
# weighted national housing value. Affordability is measured as:
#   annual housing value / annual median household income
# Lower ratios mean the displayed housing price is smaller relative to income.
get_affordability_history <- function(states = NULL) {
  con <- get_db_connection()
  # Close the database connection when the helper exits.
  on.exit(dbDisconnect(con), add = TRUE)

  # State rows are limited to selected states; if none are selected, only the
  # weighted average line is returned.
  state_filter <- "FALSE"
  if (!is.null(states) && length(states) > 0) {
    quoted_states <- paste(dbQuoteString(con, states), collapse = ", ")
    state_filter <- paste0("state IN (", quoted_states, ")")
  }

  query <- paste(
    "WITH annual_housing AS (",
    "  SELECT",
    "    state,",
    "    DATE_PART('year', date)::integer AS observation_year,",
    "    AVG(COALESCE(zhvi, price)) AS housing_value",
    "  FROM housing_prices",
    "  WHERE state IS NOT NULL",
    "    AND date IS NOT NULL",
    "    AND COALESCE(zhvi, price) IS NOT NULL",
    "  GROUP BY state, DATE_PART('year', date)::integer",
    "), state_annual_housing AS (",
    "  SELECT",
    "    state AS dataset,",
    "    observation_year,",
    "    AVG(housing_value) AS housing_value",
    "  FROM annual_housing",
    "  WHERE", state_filter,
    "  GROUP BY state, observation_year",
    "), weighted_annual_housing AS (",
    "  SELECT",
    "    'Weighted Average Housing Price' AS dataset,",
    "    h.observation_year,",
    "    SUM(h.housing_value * u.revised_housing_units::numeric) /",
    "      NULLIF(SUM(u.revised_housing_units::numeric), 0) AS housing_value",
    "  FROM annual_housing h",
    "  JOIN public.total_housing_units_by_state u",
    "    ON u.state_or_territory = h.state",
    "  WHERE u.revised_housing_units IS NOT NULL",
    "    AND u.revised_housing_units > 0",
    "  GROUP BY h.observation_year",
    "), housing_sources AS (",
    "  SELECT dataset, observation_year, housing_value",
    "  FROM state_annual_housing",
    "  UNION ALL",
    "  SELECT dataset, observation_year, housing_value",
    "  FROM weighted_annual_housing",
    "), annual_income AS (",
    "  SELECT",
    "    DATE_PART('year', d.observation_date)::integer AS observation_year,",
    "    AVG(io.observation_value)::numeric AS income_value",
    "  FROM income.income_observation io",
    "  JOIN income.income_series s",
    "    ON s.series_id = io.series_id",
    "  JOIN income.observation_date d",
    "    ON d.date_id = io.date_id",
    "  WHERE s.series_code = 'MEHOINUSA646N'",
    "    AND d.observation_date IS NOT NULL",
    "    AND io.observation_value IS NOT NULL",
    "  GROUP BY DATE_PART('year', d.observation_date)::integer",
    ")",
    "SELECT",
    "  hs.dataset,",
    "  MAKE_DATE(hs.observation_year, 1, 1) AS date,",
    "  hs.observation_year,",
    "  hs.housing_value,",
    "  ai.income_value,",
    "  hs.housing_value / NULLIF(ai.income_value, 0) AS affordability_ratio",
    "FROM housing_sources hs",
    "JOIN annual_income ai",
    "  ON ai.observation_year = hs.observation_year",
    "WHERE hs.housing_value IS NOT NULL",
    "  AND ai.income_value IS NOT NULL",
    "ORDER BY hs.dataset, hs.observation_year;"
  )

  result <- dbGetQuery(con, query)

  if (nrow(result) == 0) {
    return(result)
  }

  result$date <- as.Date(result$date)
  result$observation_year <- as.integer(result$observation_year)
  result$housing_value <- as.numeric(result$housing_value)
  result$income_value <- as.numeric(result$income_value)
  result$affordability_ratio <- as.numeric(result$affordability_ratio)
  result
}

# Build a consistent empty result object for Shiny validation messages.
empty_affordability_forecast_result <- function(message) {
  list(
    history = data.frame(),
    forecast = data.frame(),
    metrics = data.frame(message = message),
    message = message
  )
}

# Safely extract one confidence interval column from a forecast object.
get_forecast_interval <- function(forecast_result, interval_name, bound_name) {
  interval_values <- forecast_result[[bound_name]]

  if (is.null(interval_values) || ncol(interval_values) == 0) {
    return(rep(NA_real_, length(forecast_result$mean)))
  }

  if (interval_name %in% colnames(interval_values)) {
    return(as.numeric(interval_values[, interval_name]))
  }

  rep(NA_real_, length(forecast_result$mean))
}

# Calculate model accuracy against a holdout period from the end of the series.
calculate_forecast_metrics <- function(values, start_year, model_name, test_years) {
  test_size <- min(test_years, length(values) - 4)

  if (test_size < 1) {
    return(data.frame())
  }

  train_values <- values[seq_len(length(values) - test_size)]
  test_values <- values[(length(values) - test_size + 1):length(values)]
  train_ts <- stats::ts(train_values, start = start_year, frequency = 1)

  fitted_model <- tryCatch(
    if (model_name == "ARIMA") {
      forecast::auto.arima(train_ts)
    } else {
      forecast::ets(train_ts)
    },
    error = function(error) NULL
  )

  if (is.null(fitted_model)) {
    return(data.frame())
  }

  predictions <- tryCatch(
    as.numeric(forecast::forecast(fitted_model, h = test_size)$mean),
    error = function(error) rep(NA_real_, test_size)
  )

  errors <- predictions - test_values

  data.frame(
    model = model_name,
    train_years = length(train_values),
    test_years = test_size,
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Fit ARIMA and ETS models for one annual affordability series.
fit_affordability_series <- function(series_data, horizon_years = 5, test_years = 3) {
  series_data <- series_data[order(series_data$observation_year), ]
  series_data <- series_data[is.finite(series_data$affordability_ratio), ]

  if (nrow(series_data) < 6) {
    return(NULL)
  }

  years <- series_data$observation_year
  values <- series_data$affordability_ratio
  dataset_name <- series_data$dataset[1]
  start_year <- min(years)
  end_year <- max(years)
  history_ts <- stats::ts(values, start = start_year, frequency = 1)

  history <- data.frame(
    dataset = dataset_name,
    model = "Historical",
    date = as.Date(sprintf("%d-01-01", years)),
    observation_year = years,
    value = values,
    lower80 = NA_real_,
    upper80 = NA_real_,
    lower95 = NA_real_,
    upper95 = NA_real_,
    record_type = "Historical",
    stringsAsFactors = FALSE
  )

  metric_rows <- rbind(
    calculate_forecast_metrics(values, start_year, "ARIMA", test_years),
    calculate_forecast_metrics(values, start_year, "ETS", test_years)
  )

  if (nrow(metric_rows) > 0) {
    metric_rows$dataset <- dataset_name
    metric_rows <- metric_rows[
      ,
      c("dataset", "model", "train_years", "test_years", "rmse", "mae")
    ]
  }

  forecast_rows <- data.frame()
  model_names <- c("ARIMA", "ETS")

  for (model_name in model_names) {
    fitted_model <- tryCatch(
      if (model_name == "ARIMA") {
        forecast::auto.arima(history_ts)
      } else {
        forecast::ets(history_ts)
      },
      error = function(error) NULL
    )

    if (is.null(fitted_model)) {
      next
    }

    forecast_result <- tryCatch(
      forecast::forecast(fitted_model, h = horizon_years, level = c(80, 95)),
      error = function(error) NULL
    )

    if (is.null(forecast_result)) {
      next
    }

    forecast_years <- seq(end_year + 1, end_year + horizon_years)
    forecast_rows <- rbind(
      forecast_rows,
      data.frame(
        dataset = dataset_name,
        model = model_name,
        date = as.Date(sprintf("%d-01-01", forecast_years)),
        observation_year = forecast_years,
        value = as.numeric(forecast_result$mean),
        lower80 = get_forecast_interval(forecast_result, "80%", "lower"),
        upper80 = get_forecast_interval(forecast_result, "80%", "upper"),
        lower95 = get_forecast_interval(forecast_result, "95%", "lower"),
        upper95 = get_forecast_interval(forecast_result, "95%", "upper"),
        record_type = "Forecast",
        stringsAsFactors = FALSE
      )
    )
  }

  list(
    history = history,
    forecast = forecast_rows,
    metrics = metric_rows
  )
}

# Build all forecast outputs for the selected state list.
get_affordability_forecast_results <- function(states = NULL, horizon_years = 5, test_years = 3) {
  if (is.null(horizon_years) || length(horizon_years) == 0 || is.na(horizon_years)) {
    horizon_years <- 5
  }

  if (is.null(test_years) || length(test_years) == 0 || is.na(test_years)) {
    test_years <- 3
  }

  horizon_years <- max(1, min(10, as.integer(horizon_years)))
  test_years <- max(1, min(5, as.integer(test_years)))

  history_data <- tryCatch(
    get_affordability_history(states = states),
    error = function(error) {
      result <- data.frame()
      attr(result, "message") <- conditionMessage(error)
      result
    }
  )

  error_message <- attr(history_data, "message")
  if (!is.null(error_message)) {
    return(empty_affordability_forecast_result(error_message))
  }

  if (nrow(history_data) == 0) {
    return(empty_affordability_forecast_result(
      "No affordability history was found. Run the housing and income SQL files before forecasting."
    ))
  }

  series_results <- lapply(
    split(history_data, history_data$dataset),
    fit_affordability_series,
    horizon_years = horizon_years,
    test_years = test_years
  )
  series_results <- series_results[!vapply(series_results, is.null, logical(1))]
  series_results <- series_results[vapply(
    series_results,
    function(result) nrow(result$forecast) > 0,
    logical(1)
  )]

  if (length(series_results) == 0) {
    return(empty_affordability_forecast_result(
      "Not enough annual affordability observations were found to fit ARIMA or ETS models."
    ))
  }

  metric_results <- lapply(series_results, `[[`, "metrics")
  metric_results <- metric_results[vapply(metric_results, nrow, integer(1)) > 0]

  list(
    history = do.call(rbind, lapply(series_results, `[[`, "history")),
    forecast = do.call(rbind, lapply(series_results, `[[`, "forecast")),
    metrics = if (length(metric_results) > 0) do.call(rbind, metric_results) else data.frame(),
    message = NULL
  )
}

# Build a consistent empty result object for income modeling impact displays.
empty_income_model_impact_result <- function(message) {
  list(
    history = data.frame(),
    forecast = data.frame(),
    metrics = data.frame(message = message),
    message = message
  )
}

# Convert the clean/malformed income plot rows into one annual value per data set.
prepare_income_modeling_history <- function(income_data) {
  if (nrow(income_data) == 0) {
    return(data.frame())
  }

  income_data$date <- as.Date(income_data$date)
  income_data$value <- as.numeric(income_data$zhvi)
  income_data$observation_year <- as.integer(format(income_data$date, "%Y"))
  income_data <- income_data[
    !is.na(income_data$dataset) &
      !is.na(income_data$observation_year) &
      is.finite(income_data$value),
  ]

  if (nrow(income_data) == 0) {
    return(data.frame())
  }

  annual_values <- aggregate(
    value ~ dataset + observation_year,
    data = income_data,
    FUN = mean
  )
  annual_values$date <- as.Date(sprintf("%d-01-01", annual_values$observation_year))
  annual_values[order(annual_values$dataset, annual_values$observation_year), ]
}

# Fit one prediction model to a training series and forecast the requested years.
fit_income_prediction_model <- function(model_name, years, values, forecast_years) {
  start_year <- min(years)
  history_ts <- stats::ts(values, start = start_year, frequency = 1)

  if (model_name == "Linear Regression") {
    model_data <- data.frame(year = years, value = values)
    future_data <- data.frame(year = forecast_years)
    fitted_model <- tryCatch(
      stats::lm(value ~ year, data = model_data),
      error = function(error) NULL
    )

    if (is.null(fitted_model)) {
      return(NULL)
    }

    prediction_80 <- tryCatch(
      stats::predict(fitted_model, newdata = future_data, interval = "prediction", level = 0.80),
      error = function(error) NULL
    )
    prediction_95 <- tryCatch(
      stats::predict(fitted_model, newdata = future_data, interval = "prediction", level = 0.95),
      error = function(error) NULL
    )

    if (is.null(prediction_80) || is.null(prediction_95)) {
      return(NULL)
    }

    return(data.frame(
      value = as.numeric(prediction_95[, "fit"]),
      lower80 = as.numeric(prediction_80[, "lwr"]),
      upper80 = as.numeric(prediction_80[, "upr"]),
      lower95 = as.numeric(prediction_95[, "lwr"]),
      upper95 = as.numeric(prediction_95[, "upr"]),
      stringsAsFactors = FALSE
    ))
  }

  fitted_model <- tryCatch(
    if (model_name == "ARIMA") {
      forecast::auto.arima(history_ts)
    } else {
      forecast::ets(history_ts)
    },
    error = function(error) NULL
  )

  if (is.null(fitted_model)) {
    return(NULL)
  }

  forecast_result <- tryCatch(
    forecast::forecast(fitted_model, h = length(forecast_years), level = c(80, 95)),
    error = function(error) NULL
  )

  if (is.null(forecast_result)) {
    return(NULL)
  }

  data.frame(
    value = as.numeric(forecast_result$mean),
    lower80 = get_forecast_interval(forecast_result, "80%", "lower"),
    upper80 = get_forecast_interval(forecast_result, "80%", "upper"),
    lower95 = get_forecast_interval(forecast_result, "95%", "lower"),
    upper95 = get_forecast_interval(forecast_result, "95%", "upper"),
    stringsAsFactors = FALSE
  )
}

# Compare one income model against a holdout period at the end of the series.
calculate_income_model_metrics <- function(years, values, model_name, test_years) {
  test_size <- min(test_years, length(values) - 4)

  if (test_size < 1) {
    return(data.frame())
  }

  train_index <- seq_len(length(values) - test_size)
  test_index <- (length(values) - test_size + 1):length(values)
  train_years <- years[train_index]
  train_values <- values[train_index]
  test_year_values <- years[test_index]
  test_values <- values[test_index]

  predictions <- fit_income_prediction_model(
    model_name = model_name,
    years = train_years,
    values = train_values,
    forecast_years = test_year_values
  )

  if (is.null(predictions) || nrow(predictions) == 0) {
    return(data.frame())
  }

  errors <- predictions$value - test_values

  data.frame(
    model = model_name,
    train_years = length(train_values),
    test_years = test_size,
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    mae = mean(abs(errors), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Fit regression, ARIMA, and ETS forecasts for one clean or malformed income series.
fit_income_model_impact_series <- function(series_data, horizon_years = 5, test_years = 3) {
  series_data <- series_data[order(series_data$observation_year), ]
  series_data <- series_data[is.finite(series_data$value), ]

  if (nrow(series_data) < 6) {
    return(NULL)
  }

  dataset_name <- series_data$dataset[1]
  years <- series_data$observation_year
  values <- series_data$value
  end_year <- max(years)
  forecast_years <- seq(end_year + 1, end_year + horizon_years)
  model_names <- c("Linear Regression", "ARIMA", "ETS")

  history <- data.frame(
    dataset = dataset_name,
    model = "Historical",
    date = as.Date(sprintf("%d-01-01", years)),
    observation_year = years,
    value = values,
    lower80 = NA_real_,
    upper80 = NA_real_,
    lower95 = NA_real_,
    upper95 = NA_real_,
    record_type = "Historical",
    stringsAsFactors = FALSE
  )

  metric_rows <- do.call(
    rbind,
    lapply(
      model_names,
      function(model_name) {
        calculate_income_model_metrics(
          years = years,
          values = values,
          model_name = model_name,
          test_years = test_years
        )
      }
    )
  )

  if (!is.null(metric_rows) && nrow(metric_rows) > 0) {
    metric_rows$dataset <- dataset_name
    metric_rows <- metric_rows[
      ,
      c("dataset", "model", "train_years", "test_years", "rmse", "mae")
    ]
  } else {
    metric_rows <- data.frame()
  }

  forecast_rows <- data.frame()

  for (model_name in model_names) {
    predictions <- fit_income_prediction_model(
      model_name = model_name,
      years = years,
      values = values,
      forecast_years = forecast_years
    )

    if (is.null(predictions) || nrow(predictions) == 0) {
      next
    }

    forecast_rows <- rbind(
      forecast_rows,
      data.frame(
        dataset = dataset_name,
        model = model_name,
        date = as.Date(sprintf("%d-01-01", forecast_years)),
        observation_year = forecast_years,
        value = predictions$value,
        lower80 = predictions$lower80,
        upper80 = predictions$upper80,
        lower95 = predictions$lower95,
        upper95 = predictions$upper95,
        record_type = "Forecast",
        stringsAsFactors = FALSE
      )
    )
  }

  list(
    history = history,
    forecast = forecast_rows,
    metrics = metric_rows
  )
}

# Build clean and malformed income forecast outputs for the dashboard.
get_income_model_impact_results <- function(horizon_years = 5, test_years = 3) {
  if (is.null(horizon_years) || length(horizon_years) == 0 || is.na(horizon_years)) {
    horizon_years <- 5
  }

  if (is.null(test_years) || length(test_years) == 0 || is.na(test_years)) {
    test_years <- 3
  }

  horizon_years <- max(1, min(10, as.integer(horizon_years)))
  test_years <- max(1, min(5, as.integer(test_years)))

  income_data <- tryCatch(
    get_income_visualization_data(),
    error = function(error) {
      result <- data.frame()
      attr(result, "message") <- conditionMessage(error)
      result
    }
  )

  error_message <- attr(income_data, "message")
  if (!is.null(error_message)) {
    return(empty_income_model_impact_result(error_message))
  }

  history_data <- prepare_income_modeling_history(income_data)

  if (nrow(history_data) == 0) {
    return(empty_income_model_impact_result(
      "No clean or malformed income data was found for model-impact forecasting."
    ))
  }

  series_results <- lapply(
    split(history_data, history_data$dataset),
    fit_income_model_impact_series,
    horizon_years = horizon_years,
    test_years = test_years
  )
  series_results <- series_results[!vapply(series_results, is.null, logical(1))]
  series_results <- series_results[vapply(
    series_results,
    function(result) nrow(result$forecast) > 0,
    logical(1)
  )]

  if (length(series_results) == 0) {
    return(empty_income_model_impact_result(
      "Not enough income observations were found to fit regression, ARIMA, or ETS models."
    ))
  }

  metric_results <- lapply(series_results, `[[`, "metrics")
  metric_results <- metric_results[vapply(metric_results, nrow, integer(1)) > 0]

  list(
    history = do.call(rbind, lapply(series_results, `[[`, "history")),
    forecast = do.call(rbind, lapply(series_results, `[[`, "forecast")),
    metrics = if (length(metric_results) > 0) do.call(rbind, metric_results) else data.frame(),
    message = NULL
  )
}

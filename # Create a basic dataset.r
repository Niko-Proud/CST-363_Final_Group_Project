# Create a dataset from the database query result
source("db_connect.r")

required_columns <- c("date", "zhvi")
missing_columns <- setdiff(required_columns, names(result))
if (length(missing_columns) > 0) {
  stop(
    "Database query must return columns: ",
    paste(required_columns, collapse = ", "),
    ". Missing: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

data <- data.frame(
  date = as.Date(result$date),
  zhvi = as.numeric(result$zhvi)
)
data <- data[complete.cases(data), ]

if (nrow(data) == 0) {
  stop("No valid date/zhvi rows to plot. Check the database query result.", call. = FALSE)
}

average_data <- aggregate(zhvi ~ date, data = data, FUN = mean)

print(head(average_data))

plot_average_zhvi <- function(plot_data) {
  plot(plot_data$date, plot_data$zhvi,
       main = "Average Housing Price Over Time",
       xlab = "Date",
       ylab = "Average ZHVI",
       type = "l",
       col = "steelblue",
       lwd = 2)

  points(plot_data$date, plot_data$zhvi,
         pch = 19,
         col = "darkorange",
         cex = 0.6)
}

# Show the plot in RStudio/interactive R.
if (interactive()) {
  plot_average_zhvi(average_data)
}

# Save the plot so it is visible even when the script is run non-interactively.
png("average_housing_price_over_time.png", width = 1000, height = 650, res = 120)
plot_average_zhvi(average_data)
invisible(dev.off())

message("Plot saved to average_housing_price_over_time.png")

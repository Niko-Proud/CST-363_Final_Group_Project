library(DBI)
library(RPostgres)

get_plot_data <- function() {
  # Connect to PostgreSQL
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "housing_analysis",
    host = "localhost",
    port = 5431,
    user = "postgres",
    password = "ott3r"
  )
  on.exit(dbDisconnect(con), add = TRUE)

  suppressMessages(dbExecute(con, "
    CREATE TABLE IF NOT EXISTS housing_prices (
      id SERIAL PRIMARY KEY,
      date DATE NOT NULL,
      price NUMERIC,
      zhvi NUMERIC
    );
  "))

  suppressMessages(dbExecute(con, "
    ALTER TABLE housing_prices
    ADD COLUMN IF NOT EXISTS price NUMERIC;
  "))

  suppressMessages(dbExecute(con, "
    ALTER TABLE housing_prices
    ADD COLUMN IF NOT EXISTS zhvi NUMERIC;
  "))

  dbExecute(con, "
    INSERT INTO housing_prices (date, price, zhvi)
    SELECT date, zhvi, zhvi
    FROM (
      VALUES
        ('2024-01-01'::date, 325000),
        ('2024-02-01'::date, 331500),
        ('2024-03-01'::date, 338200),
        ('2024-04-01'::date, 336800)
    ) AS sample_data(date, zhvi)
    WHERE NOT EXISTS (
      SELECT 1
      FROM housing_prices
      WHERE COALESCE(zhvi, price) IS NOT NULL
    );
  ")

  dbGetQuery(con, "
    SELECT date, COALESCE(zhvi, price) AS zhvi
    FROM housing_prices
    ORDER BY date;
  ")
}

result <- get_plot_data()

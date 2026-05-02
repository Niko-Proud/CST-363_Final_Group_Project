-- R-friendly PostgreSQL queries.
-- Run this file after 03_normalize_staged_income_data.sql.
--
-- Correct result in R:
--   Dates should come back as date values and measurements should come back as
--   numeric values. The clean schema should not create duplicate points for the
--   same series/date pair.

-- 1. Long/tidy format.
-- Best default for R plotting with ggplot2 or grouped summaries.
-- Result shape: many rows, with series_code telling R which line/group it is.
SELECT
  observation_date,
  observation_year,
  observation_month,
  observation_quarter,
  series_code,
  series_name,
  frequency_code,
  units,
  observation_value
FROM income.v_income_observations_long
ORDER BY observation_date, series_code;

-- 2. Wide format.
-- Convenient for base R plotting or correlation checks.
-- Result shape: one row per date, with separate columns for each income series.
-- Blank/null values are normal where a quarterly or annual series has no value.
SELECT
  observation_date,
  observation_year,
  observation_month,
  observation_quarter,
  disposable_personal_income,
  fodsp,
  median_household_income,
  personal_income,
  real_personal_income
FROM income.v_income_observations_wide
ORDER BY observation_date;

-- 3. Annual summary.
-- Useful for dashboards or simple line plots by year.
-- Result shape: one row per year and series, with average/min/max/count.
SELECT
  observation_year,
  series_code,
  series_name,
  average_value,
  minimum_value,
  maximum_value,
  observation_count
FROM income.v_income_annual_summary
ORDER BY observation_year, series_code;

-- 4. Query matching the current R plotting script.
-- The R script expects columns named date and zhvi.
-- Here zhvi is just an alias for the selected income value.
-- Result shape: one date and one numeric value per median-income observation.
SELECT
  observation_date AS date,
  observation_value AS zhvi
FROM income.v_income_observations_long
WHERE series_code = 'MEHOINUSA646N'
ORDER BY observation_date;

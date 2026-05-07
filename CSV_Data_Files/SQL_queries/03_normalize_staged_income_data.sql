-- Step 03: move populated staging rows into the normalized income schema.
-- Run this file top-to-bottom after 02_create_and_populate_staging_tables.sql.
--
-- Correct result:
--   income.observation_date and income.income_observation should be populated
--   for the 2000-2024 project range. The clean fact table should have one row
--   per series/date pair.

DO $$
DECLARE
  dspi_count BIGINT;
  fodsp_count BIGINT;
  mehoinusa646n_count BIGINT;
  pi_count BIGINT;
  rpi_count BIGINT;
BEGIN
  SELECT COUNT(*) INTO dspi_count FROM income_stage.dspi;
  SELECT COUNT(*) INTO fodsp_count FROM income_stage.fodsp;
  SELECT COUNT(*) INTO mehoinusa646n_count FROM income_stage.mehoinusa646n;
  SELECT COUNT(*) INTO pi_count FROM income_stage.pi;
  SELECT COUNT(*) INTO rpi_count FROM income_stage.rpi;

  IF dspi_count = 0 THEN
    RAISE EXCEPTION 'income_stage.dspi is empty. Run 02_create_and_populate_staging_tables.sql before this file.';
  END IF;

  IF fodsp_count = 0 THEN
    RAISE EXCEPTION 'income_stage.fodsp is empty. Run 02_create_and_populate_staging_tables.sql before this file.';
  END IF;

  IF mehoinusa646n_count = 0 THEN
    RAISE EXCEPTION 'income_stage.mehoinusa646n is empty. Run 02_create_and_populate_staging_tables.sql before this file.';
  END IF;

  IF pi_count = 0 THEN
    RAISE EXCEPTION 'income_stage.pi is empty. Run 02_create_and_populate_staging_tables.sql before this file.';
  END IF;

  IF rpi_count = 0 THEN
    RAISE EXCEPTION 'income_stage.rpi is empty. Run 02_create_and_populate_staging_tables.sql before this file.';
  END IF;
END
$$;

-- raw_values stacks the five staging tables into one common shape:
--   series_code, file_name, observation_date, observation_value
WITH raw_values AS (
  SELECT
    'DSPI' AS series_code,
    'DSPI.csv' AS file_name,
    observation_date,
    dspi AS observation_value
  FROM income_stage.dspi

  UNION ALL

  SELECT
    'FODSP' AS series_code,
    'FODSP.csv' AS file_name,
    observation_date,
    fodsp AS observation_value
  FROM income_stage.fodsp

  UNION ALL

  SELECT
    'MEHOINUSA646N' AS series_code,
    'MEHOINUSA646N.csv' AS file_name,
    observation_date,
    mehoinusa646n AS observation_value
  FROM income_stage.mehoinusa646n

  UNION ALL

  SELECT
    'PI' AS series_code,
    'PI.csv' AS file_name,
    observation_date,
    pi AS observation_value
  FROM income_stage.pi

  UNION ALL

  SELECT
    'RPI' AS series_code,
    'RPI.csv' AS file_name,
    observation_date,
    rpi AS observation_value
  FROM income_stage.rpi
),
project_dates AS (
  SELECT DISTINCT
    observation_date
  FROM raw_values
  WHERE observation_value IS NOT NULL
    AND observation_date BETWEEN DATE '2000-01-01' AND DATE '2024-12-31'
)
INSERT INTO income.observation_date (
  observation_date,
  observation_year,
  observation_month,
  observation_quarter
)
SELECT
  observation_date,
  EXTRACT(YEAR FROM observation_date)::SMALLINT AS observation_year,
  EXTRACT(MONTH FROM observation_date)::SMALLINT AS observation_month,
  EXTRACT(QUARTER FROM observation_date)::SMALLINT AS observation_quarter
FROM project_dates
ON CONFLICT (observation_date) DO UPDATE
SET
  observation_year = EXCLUDED.observation_year,
  observation_month = EXCLUDED.observation_month,
  observation_quarter = EXCLUDED.observation_quarter;

-- Insert the actual values into the final fact table.
-- ON CONFLICT updates existing values instead of creating duplicate clean rows.
WITH raw_values AS (
  SELECT
    'DSPI' AS series_code,
    'DSPI.csv' AS file_name,
    observation_date,
    dspi AS observation_value
  FROM income_stage.dspi

  UNION ALL

  SELECT
    'FODSP' AS series_code,
    'FODSP.csv' AS file_name,
    observation_date,
    fodsp AS observation_value
  FROM income_stage.fodsp

  UNION ALL

  SELECT
    'MEHOINUSA646N' AS series_code,
    'MEHOINUSA646N.csv' AS file_name,
    observation_date,
    mehoinusa646n AS observation_value
  FROM income_stage.mehoinusa646n

  UNION ALL

  SELECT
    'PI' AS series_code,
    'PI.csv' AS file_name,
    observation_date,
    pi AS observation_value
  FROM income_stage.pi

  UNION ALL

  SELECT
    'RPI' AS series_code,
    'RPI.csv' AS file_name,
    observation_date,
    rpi AS observation_value
  FROM income_stage.rpi
)
INSERT INTO income.income_observation (
  series_id,
  date_id,
  source_file_id,
  observation_value
)
SELECT
  s.series_id,
  d.date_id,
  sf.source_file_id,
  rv.observation_value
FROM raw_values rv
JOIN income.income_series s
  ON s.series_code = rv.series_code
JOIN income.observation_date d
  ON d.observation_date = rv.observation_date
JOIN income.source_file sf
  ON sf.file_name = rv.file_name
WHERE rv.observation_value IS NOT NULL
  AND rv.observation_date BETWEEN DATE '2000-01-01' AND DATE '2024-12-31'
ON CONFLICT (series_id, date_id) DO UPDATE
SET
  source_file_id = EXCLUDED.source_file_id,
  observation_value = EXCLUDED.observation_value;

SELECT
  'income.income_observation' AS populated_table,
  COUNT(*) AS row_count
FROM income.income_observation;

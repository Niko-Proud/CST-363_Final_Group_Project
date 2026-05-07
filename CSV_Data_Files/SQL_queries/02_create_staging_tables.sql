-- Step 02: create empty staging tables.
-- Run this script while connected to final_project_housing_analysis.
-- Run this file top-to-bottom before importing the CSV files.
--
-- Correct result:
--   The income_stage schema should contain five empty tables whose columns
--   match the CSV headers. After this file runs, import the CSV files into
--   these tables, then run 03_normalize_staged_income_data.sql.

CREATE SCHEMA IF NOT EXISTS income_stage;

CREATE TABLE IF NOT EXISTS income_stage.dspi (
  observation_date DATE,
  dspi NUMERIC
);

CREATE TABLE IF NOT EXISTS income_stage.fodsp (
  observation_date DATE,
  fodsp NUMERIC
);

CREATE TABLE IF NOT EXISTS income_stage.mehoinusa646n (
  observation_date DATE,
  mehoinusa646n NUMERIC
);

CREATE TABLE IF NOT EXISTS income_stage.pi (
  observation_date DATE,
  pi NUMERIC
);

CREATE TABLE IF NOT EXISTS income_stage.rpi (
  observation_date DATE,
  rpi NUMERIC
);

-- This clears only the staging tables. It does not erase the final income
-- schema. Run this before importing fresh CSVs.
TRUNCATE TABLE
  income_stage.dspi,
  income_stage.fodsp,
  income_stage.mehoinusa646n,
  income_stage.pi,
  income_stage.rpi;

-- Import these CSVs into the matching tables:
--
--   CSV_Data_Files/DSPI.csv          -> income_stage.dspi
--   CSV_Data_Files/FODSP.csv         -> income_stage.fodsp
--   CSV_Data_Files/MEHOINUSA646N.csv -> income_stage.mehoinusa646n
--   CSV_Data_Files/PI.csv            -> income_stage.pi
--   CSV_Data_Files/RPI.csv           -> income_stage.rpi
--
-- Import settings:
--   Format: CSV
--   Header: Yes
--   Delimiter: ,
--   Quote: "

SELECT
  'Staging tables are ready. Import the CSV files, then run 03_normalize_staged_income_data.sql.' AS next_step;

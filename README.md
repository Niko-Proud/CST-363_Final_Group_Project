# Overview
This project shows a data analysis pipeline that includes database management, SQL querying, forecasting modeling, 
and interactive visualization. We learned a lot about several technologies but through it all, we saw how databases 
serve as the foundation for analytical systems by organizing, storing, and efficiently retrieving structured data that 
can be transformed into meaningful visualizations and predictive insights. 

# Appendix: Installation and Configuration Guide

This appendix explains how to install, configure, and run the Housing Affordability Dashboard project. 
It seems like a lot of configuration because of the data gathering and setting up the database tables. 
---

# 1. Required Software

## PostgreSQL

Download PostgreSQL and pgAdmin:

https://www.postgresql.org/download/

During installation:
- Remember the PostgreSQL password you create.
- Keep the default port 5432 unless intentionally changing it.

---

## Python

## R

Download R:

https://cran.r-project.org/

# 2. Create the PostgreSQL Database

Open pgAdmin or PostgreSQL terminal.

Run:

```sql
CREATE DATABASE housing_dashboard;
```

---

# 3. Install Python Packages

Open the terminal inside the project folder and

Run:

```bash
pip3 install pandas sqlalchemy psycopg2-binary numpy
```

---

# 4. Configure Database Connection

Open the Python database configuration file

Change up the credentials:

```python
DB_NAME = "housing_dashboard"
DB_USER = "postgres"
DB_PASSWORD = "your_password"
DB_HOST = "localhost"
DB_PORT = "5432"
```

---

# 5. Import Zillow and FRED CSV Data

Place all CSV datasets into the project data folder.

Run the Zillow import script:

```bash
python3 import_zillow_data.py
```

Run the FRED income import script:

```bash
python3 import_fred_income_data.py
```

# 6. Verify Database Tables

Open pgAdmin and

Run:

```sql
SELECT * FROM housing_prices LIMIT 10;
```

Verify that the tables exist:
- housing_prices
- income_observation
- income_series
- observation_date
- malformed income tables
- the housing unit tables

---

# 7. Install Required R Packages

Open terminal.

Start R:

```bash
R
```

Install packages:

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "corrplot",
  "DBI",
  "RPostgres",
  "forecast"
))
```

Exit R:

```r
q()
```

---

# 8. Configure R Database Connection

Open `db_connect.r`.

Update database credentials:

```r
dbConnect(
  RPostgres::Postgres(),
  dbname = "housing_dashboard",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "your_password"
)
```

---

# 9. Run the Shiny Application

Navigate to the project directory.

Run:

```bash
Rscript app.R
```

OR start R manually:

```bash
R
```

Then run:

```r
shiny::runApp("app.R")
```

The dashboard will launch in a browser.

---


## In case of a missing package in R

Example:

Error```text
there is no package called 'forecast'
```

Solution:

```r
install.packages("forecast")
```
- 95% confidence intervals

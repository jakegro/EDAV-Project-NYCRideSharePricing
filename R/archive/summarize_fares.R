
library(DBI)
library(duckdb)

# Connect to DuckDB
con <- dbConnect(duckdb())

# Create the output folder
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)

# Aggregate the 81 monthly CSV files directly
# without loading them all into R
query <- "
COPY (
  SELECT
    year,
    month,
    PULocationID,
    pickup_hour,
    day_type,
    SUM(fare_sum) AS total_fare,
    SUM(trip_count) AS total_trips,
    SUM(fare_sum) / SUM(trip_count) AS avg_fare
  FROM read_csv_auto(
    'data/processed/monthly/fares_*.csv',
    union_by_name = true
  )
  GROUP BY
    year,
    month,
    PULocationID,
    pickup_hour,
    day_type
)
TO 'data/final/rideshare_summary.csv'
(HEADER, DELIMITER ',')
"

dbExecute(con, query)

dbDisconnect(con, shutdown = TRUE)

message("Summary complete!")


library(DBI)
library(duckdb)
library(dplyr)
library(readr)

# 1. Define available months
periods <- expand.grid(
  year = 2020:2026,
  month = 1:12
) |>
  filter(year < 2026 | month <= 9) |>
  arrange(year, month)

# 2. Create folders
dir.create("data/processed/monthly",
           recursive = TRUE, showWarnings = FALSE)

dir.create("data/temp",
           recursive = TRUE, showWarnings = FALSE)

# 3. Connect to DuckDB
con <- dbConnect(duckdb())

# 4. Process one month at a time
for (i in seq_len(nrow(periods))) {

  year <- periods$year[i]
  month <- periods$month[i]

  filename <- sprintf(
    "fhvhv_tripdata_%d-%02d.parquet",
    year, month
  )

  output <- sprintf(
    "data/processed/monthly/fares_%d_%02d.csv",
    year, month
  )

  # Skip months already processed
  if (file.exists(output)) next

  url <- paste0(
    "https://d37ci6vzurychx.cloudfront.net/trip-data/",
    filename
  )

  temp_file <- file.path("data/temp", filename)

  message("Processing: ", year, "-", month)

  tryCatch({

    # Download one large file at a time
    download.file(
      url, temp_file,
      mode = "wb",
      method = "libcurl"
    )

    # Aggregate inside DuckDB
    query <- sprintf("
      SELECT
        %d AS year,
        %d AS month,
        PULocationID,
        DOLocationID,
        HOUR(pickup_datetime) AS pickup_hour,
        CASE
          WHEN DAYOFWEEK(pickup_datetime) IN (0, 6)
          THEN 'Weekend'
          ELSE 'Weekday'
        END AS day_type,
        SUM(base_passenger_fare) AS fare_sum,
        COUNT(*) AS trip_count
      FROM read_parquet('%s')
      WHERE base_passenger_fare > 0
        AND trip_miles > 0
        AND PULocationID BETWEEN 1 AND 263
        AND DOLocationID BETWEEN 1 AND 263
      GROUP BY
        PULocationID,
        DOLocationID,
        pickup_hour,
        day_type
    ", year, month, normalizePath(temp_file,
                                 winslash = "/"))

    result <- dbGetQuery(con, query)

    # Save only summarized data
    write_csv(result, output)

    message("Saved: ", output)

  }, error = function(e) {
    message("Failed: ", filename,
            " - ", conditionMessage(e))
  }, finally = {
    # Delete the large temporary file
    if (file.exists(temp_file)) {
      file.remove(temp_file)
    }
  })
}

dbDisconnect(con, shutdown = TRUE)

# 5. Combine completed monthly summaries
files <- list.files(
  "data/processed/monthly",
  pattern = "^fares_\\d{4}_\\d{2}\\.csv$",
  full.names = TRUE
)

fares <- bind_rows(lapply(files, read_csv,
                          show_col_types = FALSE))

write_csv(
  fares,
  "data/processed/rideshare_fares_2020_2026.csv"
)

print(nrow(fares))
print(sort(unique(fares$year)))

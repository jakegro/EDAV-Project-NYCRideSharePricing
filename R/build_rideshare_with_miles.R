# ============================================================
# Rebuild rideshare_clean.parquet WITH distance and time
# Source: NYC TLC High Volume FHV (Uber/Lyft) trip records
# Output: data/final/rideshare_clean_miles.parquet
#   one row = year x month x pickup_zone x pickup_hour x day_type
# ============================================================

library(tidyverse)
library(arrow)

# ---- 1. SETTINGS: pick your months -------------------------
# Start small. Each month is ~400-500 MB and ~20M trips.
# Recommended: the most recent 12 months (fares rise every year,
# so a single recent period gives a fair comparison).
months_to_get <- seq(as.Date("2025-10-01"), as.Date("2026-09-01"), by = "month")

raw_dir <- "data/raw/fhvhv"
out_dir <- "data/final/monthly_summaries"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

options(timeout = 3600)  # big downloads need a long timeout

base_url <- "https://d37ci6vzurychx.cloudfront.net/trip-data/fhvhv_tripdata_"

# ---- 2. PROCESS ONE MONTH AT A TIME ------------------------
# Download -> summarise -> save small summary -> delete raw file.
# This keeps memory and disk use low.

for (m in as.character(months_to_get)) {
  
  ym       <- substr(m, 1, 7)                        # e.g. "2025-10"
  raw_file <- file.path(raw_dir, paste0("fhvhv_tripdata_", ym, ".parquet"))
  out_file <- file.path(out_dir, paste0("summary_", ym, ".parquet"))
  
  if (file.exists(out_file)) {
    cat("Already done:", ym, "\n"); next
  }
  
  cat("Downloading", ym, "...\n")
  ok <- tryCatch({
    download.file(paste0(base_url, ym, ".parquet"), raw_file, mode = "wb")
    TRUE
  }, error = function(e) { cat("  Not available yet, skipping:", ym, "\n"); FALSE })
  if (!ok) next
  
  trips <- open_dataset(raw_file)   # lazy: nothing loaded into memory yet
  
  # cbd_congestion_fee only exists from Jan 5, 2025 onward
  has_cbd <- "cbd_congestion_fee" %in% names(trips)
  
  summary_m <- trips |>
    mutate(
      cbd_fee = if (has_cbd) cbd_congestion_fee else 0
    ) |>
    mutate(
      # Total price the rider pays, EXCLUDING tips
      rider_price = base_passenger_fare +
        coalesce(tolls, 0) +
        coalesce(bcf, 0) +
        coalesce(sales_tax, 0) +
        coalesce(congestion_surcharge, 0) +
        coalesce(airport_fee, 0) +
        coalesce(cbd_fee, 0),
      pickup_hour = hour(pickup_datetime),
      wday        = wday(pickup_datetime)      # 1 = Sunday, 7 = Saturday
    ) |>
    # Basic cleaning: drop obviously bad trips
    filter(
      trip_miles > 0, trip_miles < 100,
      trip_time  > 0, trip_time  < 4 * 3600,   # under 4 hours
      base_passenger_fare > 0,
      !is.na(PULocationID)
    ) |>
    mutate(day_type = if_else(wday %in% c(1, 7), "Weekend", "Weekday")) |>
    group_by(pickup_zone = PULocationID, pickup_hour, day_type) |>
    summarise(
      total_trips     = n(),
      total_fare      = sum(rider_price),
      total_base_fare = sum(base_passenger_fare),
      total_miles     = sum(trip_miles),
      total_time_sec  = sum(trip_time),
      total_tips      = sum(coalesce(tips, 0)),
      .groups = "drop"
    ) |>
    collect() |>
    mutate(
      year  = as.integer(substr(ym, 1, 4)),
      month = as.integer(substr(ym, 6, 7))
    )
  
  write_parquet(summary_m, out_file)
  file.remove(raw_file)   # free disk space; comment out to keep raw files
  cat("  Done:", ym, "-", nrow(summary_m), "rows\n")
}

# ---- 3. COMBINE ALL MONTHS ---------------------------------
rideshare_miles <- open_dataset(out_dir) |>
  collect() |>
  mutate(
    pickup_zone      = as.integer(pickup_zone),
    pickup_hour      = as.integer(pickup_hour),
    avg_fare         = total_fare / total_trips,
    fare_per_mile    = total_fare / total_miles,
    fare_per_minute  = total_fare / (total_time_sec / 60),
    avg_miles        = total_miles / total_trips
  ) |>
  relocate(year, month, pickup_zone, pickup_hour, day_type) |>
  arrange(year, month, pickup_zone, pickup_hour, day_type)

write_parquet(
  rideshare_miles,
  "data/final/rideshare_clean_miles.parquet",
  compression = "zstd"
)

cat("\nRows:", nrow(rideshare_miles), "\n")
cat("Months:", n_distinct(paste(rideshare_miles$year, rideshare_miles$month)), "\n")
cat("Total trips:", format(sum(rideshare_miles$total_trips), big.mark = ","), "\n")
cat("Overall fare per mile: $",
    round(sum(rideshare_miles$total_fare) / sum(rideshare_miles$total_miles), 2), "\n")

library(tidyverse)
library(arrow)
library(sf)

# ============================================
# 1. LOAD ALL FOUR DATASETS
# ============================================

fares <- read_parquet(
  "data/final/rideshare_clean.parquet"
)

lookup <- read.csv(
  "data/raw/taxi_zone_lookup.csv"
)

demand <- read.csv(
  "data/raw/Pickups_and_Drop-offs_by_Taxi_Zone_and_Industry_20261119.csv"
)

zones <- read.csv(
  "data/raw/NYC_Taxi_Zones_20261119.csv"
)

cat("All four datasets loaded.\n")


# ============================================
# 2. CLEAN TAXI ZONE LOOKUP
# ============================================

lookup <- lookup |>
  transmute(
    pickup_zone = as.integer(LocationID),
    zone_name = Zone,
    borough = Borough,
    service_zone = service_zone
  ) |>
  distinct(pickup_zone, .keep_all = TRUE)

stopifnot(!anyDuplicated(lookup$pickup_zone))


# ============================================
# 3. CLEAN PICKUP AND DROP-OFF DATA
# ============================================

demand <- demand |>
  transmute(
    date = as.Date(Metric.Month),
    year = lubridate::year(date),
    month = lubridate::month(date),
    pickup_zone = as.integer(Location.ID),
    industry = Industry,
    direction = Pickup.Dropoff,
    trip_count = readr::parse_number(
      as.character(Trip.Count)
    )
  ) |>
  filter(
    industry == "FHV - High Volume",
    direction %in% c("Pick-up", "Drop-off"),
    year >= 2020,
    year <= 2026,
    !is.na(date),
    !is.na(pickup_zone),
    !is.na(trip_count)
  ) |>
  mutate(
    direction = case_when(
      direction == "Pick-up" ~ "pickups",
      direction == "Drop-off" ~ "dropoffs"
    )
  ) |>
  group_by(year, month, pickup_zone, direction) |>
  summarise(
    demand_trips = sum(trip_count),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = direction,
    values_from = demand_trips
  )

stopifnot(
  !anyDuplicated(
    demand[c("year", "month", "pickup_zone")]
  )
)

cat("Monthly demand cleaned.\n")


# ============================================
# 4. CLEAN AND MERGE FARE DATA
# ============================================

rideshare <- fares |>
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    pickup_zone = as.integer(pickup_zone),
    pickup_hour = as.integer(pickup_hour)
  ) |>
  filter(
    year >= 2020,
    year <= 2026,
    month %in% 1:12,
    pickup_hour %in% 0:23,
    total_trips > 0,
    is.finite(total_fare),
    total_fare > 0
  ) |>
  mutate(
    avg_fare = total_fare / total_trips
  ) |>
  left_join(
    lookup,
    by = "pickup_zone",
    relationship = "many-to-one"
  ) |>
  left_join(
    demand,
    by = c("year", "month", "pickup_zone"),
    relationship = "many-to-one"
  ) |>
  arrange(year, month, pickup_zone, pickup_hour)

cat("Fare, neighborhood, and demand data merged.\n")


# ============================================
# 5. CLEAN NYC TAXI ZONE GEOMETRY
# ============================================

zones <- zones |>
  transmute(
    pickup_zone = as.integer(Location.ID),
    zone_name = Zone,
    borough = Borough,
    geometry_text = Shape.Geometry
  ) |>
  distinct(pickup_zone, .keep_all = TRUE) |>
  filter(
    !is.na(geometry_text),
    nzchar(trimws(geometry_text))
  )

# Support WKT or GeoJSON geometry text
first_geometry <- trimws(zones$geometry_text[1])

if (startsWith(first_geometry, "{")) {

  if (!requireNamespace("geojsonsf", quietly = TRUE)) {
    stop(
      "GeoJSON geometry detected. Run: ",
      "install.packages('geojsonsf')"
    )
  }

  geometry <- geojsonsf::geojson_sfc(
    zones$geometry_text
  )

} else {

  geometry <- sf::st_as_sfc(
    zones$geometry_text,
    crs = 4326
  )

}

taxi_zones <- sf::st_sf(
  zones |> select(-geometry_text),
  geometry = geometry
)

if (is.na(sf::st_crs(taxi_zones))) {
  sf::st_crs(taxi_zones) <- 4326
}

taxi_zones <- sf::st_transform(
  taxi_zones,
  4326
)

cat("NYC taxi zone boundaries prepared.\n")


# ============================================
# 6. SAVE FINAL DATASETS
# ============================================

dir.create(
  "data/final",
  recursive = TRUE,
  showWarnings = FALSE
)

# Main dataset for EDA and Shiny
arrow::write_parquet(
  rideshare,
  "data/final/rideshare_merged.parquet",
  compression = "zstd"
)

# Monthly demand at its original time resolution
arrow::write_parquet(
  demand,
  "data/final/monthly_demand.parquet",
  compression = "zstd"
)

# NYC geographic map
saveRDS(
  taxi_zones,
  "data/final/taxi_zones.rds"
)


# ============================================
# 7. VALIDATE FINAL DATA
# ============================================

cat("\n----- FINAL DATA SUMMARY -----\n")

cat("Rows:", nrow(rideshare), "\n")

cat(
  "Years:",
  paste(sort(unique(rideshare$year)), collapse = ", "),
  "\n"
)

cat(
  "Unique pickup zones:",
  n_distinct(rideshare$pickup_zone),
  "\n"
)

cat(
  "Missing neighborhood names:",
  sum(is.na(rideshare$zone_name)),
  "\n"
)

cat(
  "Missing monthly demand:",
  sum(is.na(rideshare$pickups)),
  "\n"
)

cat(
  "Taxi zone polygons:",
  nrow(taxi_zones),
  "\n"
)

cat(
  "Total rides:",
  format(
    sum(rideshare$total_trips),
    big.mark = ",",
    scientific = FALSE
  ),
  "\n"
)

cat("\nCleaned datasets saved successfully.\n")

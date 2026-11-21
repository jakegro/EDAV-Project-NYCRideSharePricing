# Initial analysis and plots 
library(tidyverse)
library(arrow)
library(sf)
library(scales)

# ============================================
# 1. LOAD CLEANED DATA
# ============================================

rideshare <- read_parquet(
  "data/final/rideshare_merged.parquet"
)

taxi_zones <- readRDS(
  "data/final/taxi_zones.rds"
)

dir.create("figures", showWarnings = FALSE)

# Check the datasets
glimpse(rideshare)

cat("Total rows:", nrow(rideshare), "\n")
cat("Taxi zones:", nrow(taxi_zones), "\n")

# ============================================
# 1B. ANALYSIS SETTINGS
# ============================================

if (!"total_miles" %in% names(rideshare)) {
  stop("No total_miles column. Run build_rideshare_with_miles.R, then clean_data.R.")
}

boroughs      <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
airport_zones <- c(1, 132, 138)   # Newark, JFK, LaGuardia (extra airport fee)

rideshare <- rideshare |>
  mutate(date = as.Date(sprintf("%d-%02d-01", as.integer(year), as.integer(month))))

# Use the most recent 12 months in the data (fares rise every year)
end_month   <- max(rideshare$date)
start_month <- seq(end_month, by = "-11 months", length.out = 2)[2]
period_label <- paste(format(start_month, "%b %Y"), "-", format(end_month, "%b %Y"))

recent <- rideshare |>
  filter(
    date >= start_month,
    borough %in% boroughs,
    !pickup_zone %in% airport_zones
  )

n_months  <- n_distinct(recent$date)
min_trips <- 1000 * n_months      # at least ~1,000 trips per month per zone

cat("Analysis period:", period_label, "(", n_months, "months )\n")


# ============================================
# 1C. ZONE SUMMARY (distance-adjusted)
# ============================================
# Always sum first, then divide (weights busy hours correctly)

zone_miles <- recent |>
  group_by(pickup_zone, zone_name, borough) |>
  summarise(
    total_trips    = sum(total_trips),
    total_fare     = sum(total_fare),
    total_miles    = sum(total_miles),
    total_time_sec = sum(total_time_sec),
    .groups = "drop"
  ) |>
  mutate(
    avg_fare      = total_fare / total_trips,
    avg_miles     = total_miles / total_trips,
    fare_per_mile = total_fare / total_miles,
    fare_per_min  = total_fare / (total_time_sec / 60)
  ) |>
  filter(total_trips >= min_trips) |>
  # A few zone names repeat (e.g. Corona); add the ID only for those
  mutate(
    zone_label = if_else(
      duplicated(zone_name) | duplicated(zone_name, fromLast = TRUE),
      paste0(zone_name, " #", pickup_zone),
      zone_name
    )
  )

cat("Zones kept after minimum-trip filter:", nrow(zone_miles), "\n")

# ============================================
# 2. AVERAGE FARE BY YEAR
# ============================================

annual <- rideshare |>
  group_by(year) |>
  summarise(
    total_fare = sum(total_fare),
    total_trips = sum(total_trips),
    avg_fare = total_fare / total_trips,
    .groups = "drop"
  )

ggplot(annual, aes(x = year, y = avg_fare)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(
    aes(label = dollar(avg_fare)),
    vjust = -0.8,
    size = 3.5
  ) +
  scale_x_continuous(breaks = 2020:2026) +
  scale_y_continuous(
    labels = label_dollar(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Average NYC Rideshare Fare by Year",
    subtitle = "Base passenger fares | 2026 through September",
    x = "Year",
    y = "Average fare"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/01_annual_fares.png",
  width = 9,
  height = 5,
  dpi = 300
)


# ============================================
# 3. MONTHLY FARE TRENDS
# ============================================

monthly <- rideshare |>
  group_by(year, month) |>
  summarise(
    total_fare = sum(total_fare),
    total_trips = sum(total_trips),
    avg_fare = total_fare / total_trips,
    .groups = "drop"
  ) |>
  mutate(
    date = as.Date(
      sprintf("%d-%02d-01", year, month)
    )
  )

ggplot(monthly, aes(x = date, y = avg_fare)) +
  geom_line(
    color = "steelblue",
    linewidth = 1
  ) +
  geom_point(
    color = "steelblue",
    size = 1.5
  ) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Monthly Rideshare Fare Trends",
    subtitle = "NYC, January 2020 - September 2026",
    x = "Date",
    y = "Average base fare"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/02_monthly_fares.png",
  width = 10,
  height = 5,
  dpi = 300
)


# ============================================
# 4. AVERAGE FARE BY HOUR
# ============================================

hourly <- recent |>
  group_by(pickup_hour) |>
  summarise(
    total_fare = sum(total_fare),
    total_trips = sum(total_trips),
    avg_fare = total_fare / total_trips,
    .groups = "drop"
  )

ggplot(hourly, aes(x = pickup_hour, y = avg_fare)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Average Rideshare Fare by Pickup Hour",
    subtitle = "NYC, 2025",
    x = "Hour of day",
    y = "Average base fare"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/03_hourly_fares.png",
  width = 10,
  height = 5,
  dpi = 300
)


# ============================================
# 5. WEEKDAY VS WEEKEND FARES
# ============================================

day_comparison <- recent |>
  group_by(day_type) |>
  summarise(
    total_fare = sum(total_fare),
    total_trips = sum(total_trips),
    avg_fare = total_fare / total_trips,
    .groups = "drop"
  )

ggplot(
  day_comparison,
  aes(x = day_type, y = avg_fare, fill = day_type)
) +
  geom_col(width = 0.55) +
  geom_text(
    aes(label = dollar(avg_fare)),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(
    labels = label_dollar(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.15))
  ) +
  guides(fill = "none") +
  labs(
    title = "Weekday vs Weekend Rideshare Fares",
    subtitle = "NYC, 2025",
    x = NULL,
    y = "Average base fare"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/04_weekday_weekend.png",
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================
# 6. FARE BY PICKUP NEIGHBORHOOD
# ============================================

zone_fares <- recent |>
  group_by(pickup_zone, zone_name, borough) |>
  summarise(
    total_fare = sum(total_fare),
    total_trips = sum(total_trips),
    avg_fare = total_fare / total_trips,
    .groups = "drop"
  ) |>
  filter(
    total_trips >= 10000,
    !is.na(zone_name)
  )

# Ten lowest and ten highest average fares
zone_ranking <- bind_rows(
  zone_fares |>
    slice_min(avg_fare, n = 10, with_ties = FALSE) |>
    mutate(category = "Lowest Average Fares"),

  zone_fares |>
    slice_max(avg_fare, n = 10, with_ties = FALSE) |>
    mutate(category = "Highest Average Fares")
) |>
  mutate(
    zone_label = paste(zone_name, borough, sep = " - ")
  )

ggplot(
  zone_ranking,
  aes(
    x = fct_reorder(zone_label, avg_fare),
    y = avg_fare,
    fill = category
  )
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = dollar(avg_fare)),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip(clip = "off") +
  facet_wrap(
    ~category,
    scales = "free_y",
    ncol = 2
  ) +
  scale_y_continuous(
    labels = label_dollar(),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title = "Cheapest and Most Expensive Pickup Zones",
    subtitle = "2025 | Minimum 10,000 trips per zone",
    x = NULL,
    y = "Average base fare"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  "figures/05_zone_rankings.png",
  width = 13,
  height = 7,
  dpi = 300
)


# ============================================
# 7. TRIP VOLUME VS AVERAGE FARE
# ============================================

ggplot(
  zone_fares,
  aes(x = total_trips, y = avg_fare)
) +
  geom_point(
    aes(color = borough),
    alpha = 0.7,
    size = 2.5
  ) +
  scale_x_log10(labels = label_comma()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Trip Volume vs Average Rideshare Fare",
    subtitle = "NYC pickup zones, 2025",
    x = "Number of trips (log scale)",
    y = "Average base fare",
    color = "Borough"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/06_volume_vs_fare.png",
  width = 10,
  height = 6,
  dpi = 300
)



# ============================================
# 8. NYC TAXI ZONE HEATMAPS
# ============================================

library(leaflet)
library(htmlwidgets)

# Average fare by pickup zone for 2025
zone_fares <- rideshare |>
  filter(year == 2025) |>
  group_by(pickup_zone) |>
  summarise(
    total_fare = sum(total_fare),
    total_trips = sum(total_trips),
    avg_fare = total_fare / total_trips,
    .groups = "drop"
  ) |>
  filter(total_trips >= 10000)

# Join fare data to NYC geographic boundaries
map_data <- taxi_zones |>
  left_join(zone_fares, by = "pickup_zone") |>
  st_transform(4326)

# --------------------------------------------
# A. STATIC HEATMAP
# --------------------------------------------

static_map <- ggplot(map_data) +
  geom_sf(
    aes(fill = avg_fare),
    color = "grey40",
    linewidth = 0.08
  ) +
  scale_fill_gradient(
    low = "#2ca25f",
    high = "#de2d26",
    na.value = "grey85",
    labels = scales::label_dollar(),
    name = "Average Fare\nGreen = Lower\nRed = Higher"
  ) +
  labs(
    title = "NYC Rideshare Fare Heatmap",
    subtitle = "Average base fare by pickup zone | 2025",
    caption = "Source: NYC TLC | Grey zones have insufficient data"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(
      fill = "white", color = NA
    ),
    panel.background = element_rect(
      fill = "white", color = NA
    ),
    legend.position = "right",
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave(
  "figures/07_nyc_fare_heatmap.png",
  plot = static_map,
  width = 12,
  height = 9,
  dpi = 300,
  bg = "white"
)

# ============================================
# 9. WHY DISTANCE MATTERS: MILES VS FARE
# ============================================

ggplot(zone_miles, aes(x = avg_miles, y = avg_fare)) +
  geom_point(aes(color = borough), alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linewidth = 0.7) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Longer Trips Cost More",
    subtitle = paste("Each dot is a pickup zone |", period_label),
    x = "Average trip distance (miles)",
    y = "Average fare (excl. tips)",
    color = "Borough"
  ) +
  theme_minimal(base_size = 12)

ggsave("figures/08_miles_vs_fare.png", width = 10, height = 6, dpi = 300)


# ============================================
# 10. FARE PER MILE BY BOROUGH
# ============================================

ggplot(
  zone_miles,
  aes(x = fct_reorder(borough, fare_per_mile, .fun = median), y = fare_per_mile)
) +
  geom_boxplot(outlier.shape = NA, fill = "grey92") +
  geom_jitter(aes(color = borough), width = 0.15, alpha = 0.6, size = 1.8,
              show.legend = FALSE) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Rideshare Price per Mile by Borough",
    subtitle = paste("Each dot is a pickup zone |", period_label),
    x = NULL,
    y = "Fare per mile (excl. tips)"
  ) +
  theme_minimal(base_size = 12)

ggsave("figures/09_fare_per_mile_by_borough.png", width = 9, height = 6, dpi = 300)


# ============================================
# 11. CHEAPEST & PRICIEST ZONES IN EACH BOROUGH
# ============================================

ranked <- zone_miles |>
  group_by(borough) |>
  mutate(
    rank_low  = rank(fare_per_mile, ties.method = "first"),
    rank_high = rank(-fare_per_mile, ties.method = "first")
  ) |>
  filter(rank_low <= 5 | rank_high <= 5) |>
  mutate(group = if_else(rank_low <= 5, "5 cheapest", "5 priciest")) |>
  ungroup()

ggplot(
  ranked,
  aes(x = fare_per_mile, y = fct_reorder(zone_label, fare_per_mile))
) +
  geom_point(aes(color = group), size = 3) +
  facet_wrap(~borough, scales = "free_y", ncol = 2) +
  scale_x_continuous(labels = label_dollar()) +
  scale_color_manual(values = c("5 cheapest" = "#1b9e77", "5 priciest" = "#d95f02")) +
  labs(
    title = "Cheapest and Priciest Places to Get a Rideshare, by Borough",
    subtitle = paste("Fare per mile by pickup zone |", period_label),
    x = "Fare per mile (excl. tips)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.major.y = element_line(linetype = "dotted"))

ggsave("figures/10_cheapest_by_borough.png", width = 12, height = 10, dpi = 300)

# The direct answer, as a table
cheapest_per_borough <- zone_miles |>
  group_by(borough) |>
  slice_min(fare_per_mile, n = 1) |>
  select(borough, zone_label, fare_per_mile, avg_fare, avg_miles, total_trips)

print(cheapest_per_borough)
write_csv(cheapest_per_borough, "figures/cheapest_per_borough.csv")


# ============================================
# 12. MAP: FARE PER MILE BY PICKUP ZONE
# ============================================

map_miles <- taxi_zones |>
  select(pickup_zone, geometry) |>
  left_join(zone_miles |> select(pickup_zone, fare_per_mile), by = "pickup_zone")

ggplot(map_miles) +
  geom_sf(aes(fill = fare_per_mile), color = "grey40", linewidth = 0.08) +
  scale_fill_distiller(
    palette = "YlOrRd", direction = 1,
    na.value = "grey85", labels = label_dollar(),
    name = "Fare per mile"
  ) +
  labs(
    title = "Where Rideshares Cost the Most per Mile",
    subtitle = period_label,
    caption = "Source: NYC TLC | Grey = airports or too few trips"
  ) +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("figures/11_fare_per_mile_map.png", width = 12, height = 9, dpi = 300, bg = "white")


# ============================================
# 13. WHEN IS IT CHEAPEST? HOUR x DAY TYPE
# ============================================

hour_borough <- recent |>
  group_by(borough, day_type, pickup_hour) |>
  summarise(fare_per_mile = sum(total_fare) / sum(total_miles), .groups = "drop")

ggplot(hour_borough, aes(x = pickup_hour, y = day_type, fill = fare_per_mile)) +
  geom_tile(color = "white") +
  facet_wrap(~borough, ncol = 1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = label_dollar(),
                       name = "Fare per mile") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(
    title = "Rideshare Price per Mile by Hour",
    subtitle = period_label,
    x = "Pickup hour",
    y = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave("figures/12_hour_heatmap.png", width = 10, height = 9, dpi = 300)



# ============================================
# FINAL OUTPUT
# ============================================

cat("\nEDA COMPLETE\n")

cat("Figures generated:", length(
  list.files("figures", pattern = "^[0-9]{2}_.*\\.png$")
), "\n")

print(list.files("figures", pattern = "\\.png$"))


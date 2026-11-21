
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

# Use 2025 for neighborhood comparisons
fares_2025 <- rideshare |>
  filter(year == 2025)

# Check the datasets
glimpse(rideshare)

cat("Total rows:", nrow(rideshare), "\n")
cat("Taxi zones:", nrow(taxi_zones), "\n")


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

hourly <- fares_2025 |>
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

day_comparison <- fares_2025 |>
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

zone_fares <- fares_2025 |>
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
# 9. FINAL OUTPUT
# ============================================

cat("\nEDA COMPLETE\n")

cat("Figures generated:", length(
  list.files("figures", pattern = "^[0-9]{2}_.*\\.png$")
), "\n")

print(list.files("figures", pattern = "\\.png$"))

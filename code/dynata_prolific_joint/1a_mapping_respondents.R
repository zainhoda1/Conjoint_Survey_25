# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

# ----Load the data set----
data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

# ----zipcode cleaning----
df <- data_joint %>%
  mutate(
    home_zipcode = case_when(
      home_zipcode == "32310I" ~ "32310",
      T ~ home_zipcode
    )
  ) %>%
  select(home_zipcode, data_source) %>%
  filter(!is.na(home_zipcode)) %>%
  mutate(
    # Convert numeric input to character and pad with leading zeros to ensure 5 digits
    # e.g., 2138 becomes "02138"
    clean_zip = sprintf("%05d", as.numeric(home_zipcode))
  ) %>%
  # Remove Invalid ZIPs (NA or not 5 digits)
  filter(clean_zip %notin% c("NA", "00000"), nchar(clean_zip) == 5)

# ---- Aggregation & Geocoding----
# Count respondents per ZIP code
zip_counts_dynata <- df %>%
  filter(data_source == "dynata") %>%
  count(clean_zip, name = "respondent_count")

zip_counts_prolific <- df %>%
  filter(data_source == "prolific") %>%
  count(clean_zip, name = "respondent_count")

# Join with zipcoder database to get Lat/Lon
# zip_code_db is provided by the zipcoder package
geo_data_dynata <- zip_counts_dynata %>%
  inner_join(zipcodeR::zip_code_db, by = c("clean_zip" = "zipcode"))

geo_data_prolific <- zip_counts_prolific %>%
  inner_join(zipcodeR::zip_code_db, by = c("clean_zip" = "zipcode"))


# ---- 4. Visualization ----

# Retrieve US state boundaries for map context
us_states <- map_data("state")

# Define a function to ensure breaks are always integers
integer_breaks <- function(x) {
  unique(floor(pretty(x)))
}

## ---- prolific ----
total_zips <- nrow(geo_data_prolific) # Number of unique ZIP codes
total_respondents <- sum(geo_data_prolific$respondent_count)

map_prolific <- ggplot() +
  # Layer 1: Base map of US States
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    fill = "#f5f5f5",
    color = "gray80",
    size = 0.3
  ) +

  # Layer 2: Respondent Data Points
  # We map size and color to the count to visualize density
  geom_point(
    data = geo_data_prolific,
    aes(x = lng, y = lat, size = respondent_count, color = respondent_count),
    alpha = 0.8
  ) +

  # Styling and Scales
  scale_color_viridis_c(
    option = "viridis",
    name = "Respondent Count",
    # Force the scale to calculate integer-only breaks
    breaks = integer_breaks,
    guide = guide_colorbar(barwidth = 10, barheight = 0.5)
  ) +
  scale_size_continuous(range = c(1, 8), guide = "none") + # Hide size legend

  # Map Theme adjustments
  theme_void() + # Removes axes and standard grid
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    legend.position = "bottom",
    legend.title = element_text(vjust = 1),
    plot.background = element_rect(fill = "white", color = NA) # Ensure white background for export
  ) +

  # Coordinate limits (Focus on Continental US)
  # Adjust ylim/xlim if you need Alaska/Hawaii
  coord_fixed(xlim = c(-125, -66), ylim = c(24, 50), ratio = 1.3) +

  # Labels
  labs(
    title = "Geographic Distribution of Respondents",
    # str_glue evaluates the code inside {}
    subtitle = str_glue(
      "Prolific n = {total_respondents} (across {total_zips} unique ZIP codes)"
    )
  )

## ---- dynata ----
total_zips <- nrow(geo_data_dynata) # Number of unique ZIP codes
total_respondents <- sum(geo_data_dynata$respondent_count)
map_dynata <- ggplot() +
  # Layer 1: Base map of US States
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group),
    fill = "#f5f5f5",
    color = "gray80",
    size = 0.3
  ) +

  # Layer 2: Respondent Data Points
  # We map size and color to the count to visualize density
  geom_point(
    data = geo_data_dynata,
    aes(x = lng, y = lat, size = respondent_count, color = respondent_count),
    alpha = 0.8
  ) +

  # Styling and Scales
  scale_color_viridis_c(
    option = "viridis",
    name = "Respondent Count",
    # Force the scale to calculate integer-only breaks
    breaks = integer_breaks,
    guide = guide_colorbar(barwidth = 10, barheight = 0.5)
  ) +
  scale_size_continuous(range = c(1, 8), guide = "none") + # Hide size legend

  # Map Theme adjustments
  theme_void() + # Removes axes and standard grid
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    legend.position = "bottom",
    legend.title = element_text(vjust = 1),
    plot.background = element_rect(fill = "white", color = NA) # Ensure white background for export
  ) +

  # Coordinate limits (Focus on Continental US)
  # Adjust ylim/xlim if you need Alaska/Hawaii
  coord_fixed(xlim = c(-125, -66), ylim = c(24, 50), ratio = 1.3) +

  # Labels
  labs(
    title = "Geographic Distribution of Respondents",
    # str_glue evaluates the code inside {}
    subtitle = str_glue(
      "Dynata n = {total_respondents} (across {total_zips} unique ZIP codes)"
    )
  )

# --- 5. Output ---
# print(map_prolific)
# print(map_dynata)

# Optional: Save to file
path_images <- 'code/output/images/'
ggsave(
  paste0(path_images, "map_prolific.png"),
  plot = map_prolific,
  width = 7,
  height = 5,
  dpi = 300
)
ggsave(
  paste0(path_images, "map_dynata.png"),
  plot = map_dynata,
  width = 7,
  height = 5,
  dpi = 300
)

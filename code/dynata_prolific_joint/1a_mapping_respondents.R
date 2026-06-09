# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

if (!requireNamespace("usmap", quietly = TRUE)) {
  install.packages("usmap")
}
library(usmap)

# Load the data set----
data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
))

data_model <- data_model %>%
  filter(
    !is.na(ATT_range_anxiety) &
      !is.na(ATT_risktaker) &
      !is.na(hhincome_num_10k) &
      !is.na(EV_charger) &
      !is.na(Veh_hh_fuel) &
      !is.na(Veh_primary_range) &
      !is.na(ATT_EVB_environment) &
      !is.na(ATT_EVB_function) &
      !is.na(vehicle_typesuv)
  )

data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

data_joint

# True sample sizes before any ZIP filtering
n_total_prolific <- data_joint %>%
  filter(data_source %in% c("prolific", "prolific_round2")) %>%
  nrow()

n_total_dynata <- data_joint %>%
  filter(data_source == "dynata") %>%
  nrow()

n_total_all <- nrow(data_joint)

# zipcode cleaning----
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

# Aggregation & Geocoding----
# Count respondents per ZIP code
zip_counts_dynata <- df %>%
  filter(data_source == "dynata") %>%
  count(clean_zip, name = "respondent_count")

zip_counts_prolific <- df %>%
  filter(data_source %in% c("prolific", "prolific_round2")) %>%
  count(clean_zip, name = "respondent_count")

# Join with zipcoder database to get Lat/Lon
# zip_code_db is provided by the zipcoder package
geo_data_dynata <- zip_counts_dynata %>%
  inner_join(zipcodeR::zip_code_db, by = c("clean_zip" = "zipcode"))

geo_data_prolific <- zip_counts_prolific %>%
  inner_join(zipcodeR::zip_code_db, by = c("clean_zip" = "zipcode"))


# 4. Visualization ----

# US state boundaries in usmap projection (AK and HI repositioned as insets)
us_states <- usmap::us_map(regions = "states")

# Transform lat/lon point data to usmap projection (sf objects)
# NA lat/lng filtered before transform to suppress geom warnings; totals computed separately
geo_data_dynata_t <- usmap_transform(
  geo_data_dynata %>%
    select(lng, lat, respondent_count) %>%
    filter(!is.na(lng), !is.na(lat)),
  input_names = c("lng", "lat")
) %>%
  arrange(respondent_count)

geo_data_prolific_t <- usmap_transform(
  geo_data_prolific %>%
    select(lng, lat, respondent_count) %>%
    filter(!is.na(lng), !is.na(lat)),
  input_names = c("lng", "lat")
) %>%
  arrange(respondent_count)

# Define a function to ensure breaks are always integers
integer_breaks <- function(x) {
  unique(floor(pretty(x)))
}

max_count <- max(
  max(geo_data_prolific$respondent_count),
  max(geo_data_dynata$respondent_count)
)

## prolific ----

total_zips <- nrow(geo_data_prolific)

map_prolific <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "#f5f5f5",
    color = "gray80",
    linewidth = 0.3
  ) +
  geom_sf(
    data = geo_data_prolific_t,
    aes(size = respondent_count, color = respondent_count),
    alpha = 0.8
  ) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Respondent Count",
    limits = c(1, max_count),
    breaks = integer_breaks,
    guide = guide_colorbar(barwidth = 10, barheight = 0.5)
  ) +
  scale_size_continuous(
    range = c(0.5, 3),
    limits = c(1, max_count),
    guide = "none"
  ) +
  coord_sf() +
  theme_void() +
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Geographic Distribution of Respondents",
    subtitle = str_glue(
      "Prolific n = {n_total_prolific} (across {total_zips} unique ZIP codes)"
    )
  )

## dynata ----
total_zips <- nrow(geo_data_dynata)

map_dynata <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "#f5f5f5",
    color = "gray80",
    linewidth = 0.3
  ) +
  geom_sf(
    data = geo_data_dynata_t,
    aes(size = respondent_count, color = respondent_count),
    alpha = 0.8
  ) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Respondent Count",
    limits = c(1, max_count),
    breaks = integer_breaks,
    guide = guide_colorbar(barwidth = 10, barheight = 0.5)
  ) +
  scale_size_continuous(
    range = c(0.5, 3),
    limits = c(1, max_count),
    guide = "none"
  ) +
  coord_sf() +
  theme_void() +
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Geographic Distribution of Respondents",
    subtitle = str_glue(
      "Dynata n = {n_total_dynata} (across {total_zips} unique ZIP codes)"
    )
  )

## combine ----
library(patchwork)

map_prolific2 <- map_prolific +
  labs(title = NULL) +
  theme(legend.position = "bottom")

map_dynata2 <- map_dynata +
  labs(title = NULL) +
  theme(legend.position = "bottom")

combined_map <- (map_prolific2 | map_dynata2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Geographic Distribution of Respondents",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    )
  ) &
  theme(legend.position = "bottom")


# 5. Output ----

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

ggsave(
  paste0(path_images, "map_combined.png"),
  plot = combined_map,
  width = 14,
  height = 5,
  dpi = 300
)


# one map regardless of data sources

zip_counts_all <- df %>%
  count(clean_zip, name = "respondent_count")

geo_data_all <- zip_counts_all %>%
  inner_join(zipcodeR::zip_code_db, by = c("clean_zip" = "zipcode"))

geo_data_all_t <- usmap_transform(
  geo_data_all %>%
    select(lng, lat, respondent_count) %>%
    filter(!is.na(lng), !is.na(lat)),
  input_names = c("lng", "lat")
) %>%
  arrange(respondent_count)

total_zips_all <- nrow(geo_data_all)

map_all <- ggplot() +
  geom_sf(
    data = us_states,
    fill = "#f5f5f5",
    color = "gray80",
    linewidth = 0.3
  ) +
  geom_sf(
    data = geo_data_all_t,
    aes(size = respondent_count, color = respondent_count),
    alpha = 0.8
  ) +
  scale_color_viridis_c(
    option = "viridis",
    name = "Respondent Count",
    limits = c(1, max(geo_data_all$respondent_count)),
    breaks = integer_breaks,
    guide = guide_colorbar(barwidth = 10, barheight = 0.5)
  ) +
  scale_size_continuous(
    range = c(0.5, 3),
    limits = c(1, max(geo_data_all$respondent_count)),
    guide = "none"
  ) +
  coord_sf() +
  theme_void() +
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
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = str_glue(
      "n = {n_total_all} (across {total_zips_all} unique ZIP codes)"
    )
  ) +
  theme(
    plot.caption = element_text(
      size = 10,
      hjust = 0.5,
      color = "gray40",
      margin = margin(t = 10)
    )
  )

map_all

ggsave(
  paste0(path_images, "map_all.png"),
  plot = map_all,
  width = 7,
  height = 5,
  dpi = 300
)

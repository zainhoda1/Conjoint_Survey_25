# After running both Dynata and Prolific
source(here::here('code', 'setup.R'))

if (!requireNamespace("usmap", quietly = TRUE)) {
  install.packages("usmap")
}
library(usmap)

# Load the data set----
data_joint <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_joint.parquet"
))

data_model <- read_parquet(here(
  "data",
  "dynata_prolific_joint",
  "data_apollo_battery.parquet"
)) |> 
  select(psid) |> 
  distinct(psid)

data_joint <- data_joint |> 
  inner_join(data_model, by="psid")


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
    clean_zip = sprintf("%05d", as.numeric(home_zipcode))
  ) %>%
  filter(clean_zip %notin% c("NA", "00000"), nchar(clean_zip) == 5)

# Aggregation & Geocoding----
zip_counts_all <- df %>%
  count(clean_zip, name = "respondent_count")

geo_data_all <- zip_counts_all %>%
  inner_join(zipcodeR::zip_code_db, by = c("clean_zip" = "zipcode"))

# Visualization----
us_states <- usmap::us_map(regions = "states")

geo_data_all_t <- usmap_transform(
  geo_data_all %>%
    select(lng, lat, respondent_count) %>%
    filter(!is.na(lng), !is.na(lat)),
  input_names = c("lng", "lat")
) %>% arrange(respondent_count)

total_zips_all <- nrow(geo_data_all)

integer_breaks <- function(x) {
  unique(floor(pretty(x)))
}

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

# Output----
path_images <- 'code/output/images/'
ggsave(
  paste0(path_images, "map_all.png"),
  plot = map_all,
  width = 7,
  height = 5,
  dpi = 300
)

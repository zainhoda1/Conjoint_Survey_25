source(here::here('code', 'setup.R'))

#Load data----
ml_3c_wtp_combined <- readRDS(
  (here(
    "code",
    "output",
    "model_output",
    "battery_analysis",
    "logitr",
    "latent_class",
    "ml_3c_wtp_combined.Rds"
  ))
)


# # Extract tidy tables and plot estimates by class for car and SUV
# library(tidyverse)
# library(broom)

# make_class_plots <- function(models_list) {
#   nm <- names(models_list)
#   if (is.null(nm)) {
#     nm <- paste0("model", seq_along(models_list))
#   }
#   names(models_list) <- nm

#   tidy_one <- function(obj, name) {
#     if (is.data.frame(obj)) {
#       df <- as_tibble(obj)
#       if (!"term" %in% names(df)) {
#         df <- df %>% mutate(term = rownames(obj) %||% seq_len(nrow(df)))
#       }
#       if (!"estimate" %in% names(df) & "estimate" %in% names(df)) {
#         df <- df
#       }
#       if (!"conf.low" %in% names(df)) {
#         if ("std.error" %in% names(df)) {
#           df <- df %>%
#             mutate(
#               conf.low = estimate - 1.96 * std.error,
#               conf.high = estimate + 1.96 * std.error
#             )
#         } else if ("se" %in% names(df)) {
#           df <- df %>%
#             mutate(
#               conf.low = estimate - 1.96 * se,
#               conf.high = estimate + 1.96 * se
#             )
#         } else {
#           df <- df %>% mutate(conf.low = NA_real_, conf.high = NA_real_)
#         }
#       }
#       df %>%
#         select(term, estimate, conf.low, conf.high) %>%
#         mutate(model_name = name)
#     } else {
#       td <- tryCatch(broom::tidy(obj, conf.int = TRUE), error = function(e) {
#         broom::tidy(obj)
#       })
#       if (!"conf.low" %in% names(td) & "std.error" %in% names(td)) {
#         td <- td %>%
#           mutate(
#             conf.low = estimate - 1.96 * std.error,
#             conf.high = estimate + 1.96 * std.error
#           )
#       }
#       td %>%
#         rename(term = term, estimate = estimate) %>%
#         select(term, estimate, conf.low, conf.high) %>%
#         mutate(model_name = name)
#     }
#   }

#   tidy_all <- imap_dfr(models_list, ~ tidy_one(.x, .y))

#   # infer domain and class
#   tidy_all <- tidy_all %>%
#     mutate(
#       domain = case_when(
#         str_detect(model_name, regex("car", ignore_case = TRUE)) ~ "car",
#         str_detect(model_name, regex("suv", ignore_case = TRUE)) ~ "suv",
#         TRUE ~ NA_character_
#       )
#     )

#   if (all(is.na(tidy_all$domain))) {
#     model_names <- unique(tidy_all$model_name)
#     n_models <- length(model_names)
#     assign_domains <- rep(c("car", "suv"), length.out = n_models)
#     domain_map <- tibble(model_name = model_names, domain = assign_domains)
#     tidy_all <- left_join(tidy_all, domain_map, by = "model_name")
#   }

#   # extract class number or assign sequence within domain
#   tidy_all <- tidy_all %>%
#     mutate(
#       class = as.integer(if_else(
#         str_detect(model_name, regex("class\\D*(\\d+)", ignore_case = TRUE)),
#         str_extract(model_name, regex("(\\d+)(?!.*\\d)", ignore_case = TRUE)),
#         NA_character_
#       )),
#       model_order = as.integer(factor(model_name, levels = unique(model_name)))
#     )

#   tidy_all <- tidy_all %>%
#     group_by(domain) %>%
#     mutate(
#       class = if_else(
#         is.na(class),
#         row_number() %/% (n() / n_distinct(model_name)) + 1L,
#         class
#       ),
#       class_label = paste0("class", class)
#     ) %>%
#     ungroup()

#   # remove obvious non-feature terms (intercept) and keep common terms only
#   tidy_plot_ready <- tidy_all %>%
#     filter(
#       !str_detect(term, regex("Intercept|\\(Intercept\\)", ignore_case = TRUE))
#     )

#   plot_domain <- function(dom) {
#     df <- tidy_plot_ready %>% filter(domain == dom)
#     if (nrow(df) == 0) {
#       return(NULL)
#     }

#     term_order <- df %>%
#       group_by(term) %>%
#       summarize(med = median(estimate, na.rm = TRUE), .groups = "drop") %>%
#       arrange(med)

#     df <- df %>%
#       mutate(term = factor(term, levels = term_order$term))

#     ggplot(df, aes(x = estimate, y = term, color = class_label)) +
#       geom_point(position = position_dodge(width = 0.6), size = 2) +
#       geom_errorbarh(
#         aes(xmin = conf.low, xmax = conf.high),
#         position = position_dodge(width = 0.6),
#         height = 0.2
#       ) +
#       labs(
#         x = "Estimate (WTP / coefficient)",
#         y = NULL,
#         color = "Class",
#         title = paste(str_to_title(dom), "— estimates by class")
#       ) +
#       theme_minimal() +
#       theme(axis.text.y = element_text(size = 9))
#   }

#   list(
#     car = plot_domain("car"),
#     suv = plot_domain("suv"),
#     tidy_table = tidy_plot_ready
#   )
# }

# # Usage
# plots_and_tables <- make_class_plots(ml_3c_wtp_combined)

# # returned object: plots_and_tables$car, plots_and_tables$suv (ggplot objects)
# # and plots_and_tables$tidy_table (combined tidy data frame)
# plots_and_tables

# plots_and_tables$car

compute_wtp_surface <- function(
  model,
  vehicle_type,
  class_name,
  range_seq = seq(0, 350, by = 10),
  deg_vals = c(1, 2, 3, 4),
  n_draws = 1e4
) {
  coefs <- coef(model)
  cov_mat <- vcov(model)

  coef_draws <- as.data.frame(MASS::mvrnorm(n_draws, coefs, cov_mat))

  grid <- expand.grid(range = range_seq, degradation = deg_vals)

  map_dfr(seq_len(nrow(grid)), function(i) {
    r <- grid$range[i] / 100
    d <- grid$degradation[i]

    vals <- ((coef_draws$battery_range_year0 *
      r +
      coef_draws$battery_degradation * d)) *
      10

    tibble(
      range = grid$range[i],
      degradation = d,
      mean = mean(vals, na.rm = TRUE),
      lower = quantile(vals, 0.025, na.rm = TRUE),
      upper = quantile(vals, 0.975, na.rm = TRUE),
      vehicle_type = vehicle_type,
      class = class_name
    )
  })
}

wtp_models <- ml_3c_wtp_combined

wtp_summary_all <- imap_dfr(wtp_models, function(mod, name) {
  vehicle_type <- ifelse(grepl("car", name), "Car", "SUV")
  class_name <- gsub(".*class", "Class ", name)

  compute_wtp_surface(
    model = mod,
    vehicle_type = vehicle_type,
    class_name = class_name
  )
})

wtp_summary_all$class <- recode(
  wtp_summary_all$class,
  "Class 1" = "Class 1\nBEV-adverse",
  "Class 2" = "Class 2\nBEV-skeptical",
  "Class 3" = "Class 3\nBEV-open"
)

colors <- c(
  "1" = "#3B8945",
  "2" = "#3A7BAA",
  "3" = "#E8B74D",
  "4" = "#AC1926"
)
uncertainty_range_degradation_c3 <- ggplot(
  wtp_summary_all,
  aes(
    x = range,
    y = mean,
    color = as.factor(degradation),
    fill = as.factor(degradation)
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.3
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  facet_grid(vehicle_type ~ class) +
  scale_color_manual(
    values = colors,
    name = "Degradation Rate\n(% per year)"
  ) +
  scale_fill_manual(
    values = colors,
    name = "Degradation Rate\n(% per year)"
  ) +
  labs(
    x = "Vehicle range at year 0 (miles)",
    y = "WTP ($1000)",
    title = "WTP for Range and Battery Degradation Across Vehicle Type and Latent Classes"
  ) +
  theme_minimal()


ggsave(
  plot = uncertainty_range_degradation_c3,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_analysis",
    "logitr_latent_class",
    "uncertainty_range_degradation_c3.png"
  ),
  width = 9,
  height = 6,
  dpi = 300
)


# range, refurbishment----

compute_wtp_surface_refurb <- function(
  model,
  vehicle_type,
  class_name,
  range_seq = seq(0, 350, by = 10),
  refurb_levels = c("No", "Cell-replace", "Pack-replace"),
  n_draws = 1e4
) {
  coefs <- coef(model)
  cov_mat <- vcov(model)

  coef_draws <- as.data.frame(MASS::mvrnorm(n_draws, coefs, cov_mat))

  grid <- expand.grid(range = range_seq, refurbishment = refurb_levels)

  map_dfr(seq_len(nrow(grid)), function(i) {
    r <- grid$range[i] / 100
    refurb <- grid$refurbishment[i]

    refurb_coef <- dplyr::case_when(
      refurb == "Cell-replace" ~ coef_draws$battery_refurbishcellreplace,
      refurb == "Pack-replace" ~ coef_draws$battery_refurbishpackreplace,
      TRUE ~ 0
    )

    vals <- (coef_draws$battery_range_year0 * r + refurb_coef) * 10

    tibble::tibble(
      range = grid$range[i],
      refurbishment = refurb,
      mean = mean(vals, na.rm = TRUE),
      lower = quantile(vals, 0.025, na.rm = TRUE),
      upper = quantile(vals, 0.975, na.rm = TRUE),
      vehicle_type = vehicle_type,
      class = class_name
    )
  })
}

wtp_summary_refurb_all <- purrr::imap_dfr(wtp_models, function(mod, name) {
  vehicle_type <- ifelse(grepl("car", name), "Car", "SUV")
  class_name <- gsub(".*class", "Class ", name)

  compute_wtp_surface_refurb(
    model = mod,
    vehicle_type = vehicle_type,
    class_name = class_name
  )
})

wtp_summary_refurb_all$class <- dplyr::recode(
  wtp_summary_refurb_all$class,
  "Class 1" = "Class 1\nBEV-adverse",
  "Class 2" = "Class 2\nBEV-skeptical",
  "Class 3" = "Class 3\nBEV-open"
)

wtp_summary_refurb_all$refurbishment <- factor(
  wtp_summary_refurb_all$refurbishment,
  levels = c("No", "Cell-replace", "Pack-replace")
)

refurb_colors <- c(
  "No" = "#3A7BAA",
  "Cell-replace" = "#E8B74D",
  "Pack-replace" = "#AC1926"
)

uncertainty_range_refurb_c3 <- ggplot(
  wtp_summary_refurb_all,
  aes(
    x = range,
    y = mean,
    color = refurbishment,
    fill = refurbishment
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.3
  ) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
  facet_grid(vehicle_type ~ class) +
  scale_color_manual(
    values = refurb_colors,
    name = "Battery Refurbishment"
  ) +
  scale_fill_manual(
    values = refurb_colors,
    name = "Battery Refurbishment"
  ) +
  labs(
    x = "Vehicle range at year 0 (miles)",
    y = "WTP ($1000)",
    title = "WTP for Vehicle Range and Battery Refurbishment Across Vehicle Type and Latent Classes"
  ) +
  theme_minimal()

ggsave(
  plot = uncertainty_range_refurb_c3,
  filename = here::here(
    'code',
    'output',
    "images",
    "battery_analysis",
    "logitr_latent_class",
    "uncertainty_range_refurb_c3.png"
  ),
  width = 8,
  height = 6,
  dpi = 300
)

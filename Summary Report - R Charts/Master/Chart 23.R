# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(ggfittext)
library(grid)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "Theme",
  fullsize = FALSE
) {
  # ---- Load and prepare data ----
  data <- read_csv(source_file, show_col_types = FALSE)

  data$ret_2022 <- as.numeric(data$ret_2022)
  data$ret_2023 <- as.numeric(data$ret_2023)

  filtered_data <- data %>%
    filter(launch_year == 2022) %>%
    filter(!(is.na(ret_2022) & is.na(ret_2023)))

  retention_summary <- filtered_data %>%
    group_by(School) %>%
    summarise(
      avg_2022 = mean(ret_2022, na.rm = TRUE),
      avg_2023 = mean(ret_2023, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      percent_change = ((avg_2023 - avg_2022) / avg_2022) * 100,
      bar_color      = ifelse(percent_change >= 0, "#0554A3", "#E03244"),
      label_txt      = paste0(round(percent_change, 1), "%")
    ) %>%
    arrange(desc(percent_change))

  num_schools <- dplyr::n_distinct(retention_summary$School)

  pos_data <- retention_summary %>% filter(percent_change >= 0)
  neg_data <- retention_summary %>% filter(percent_change < 0)

  y_pad <- 5
  y_min_lim <- min(retention_summary$percent_change, na.rm = TRUE) - y_pad
  y_max_lim <- max(retention_summary$percent_change, na.rm = TRUE) + y_pad

  # ---- Build plot ----
  title_txt <- "Percentage Change in Retention\n2022 to 2023"
  subtitle_txt <- paste0("(", num_schools, " Launch Year 2022 Institutions)")

  plt <-
    ggplot(retention_summary, aes(
      x = reorder(School, percent_change),
      y = percent_change,
      fill = bar_color
    )) +
    geom_col(width = 0.7) +
    scale_fill_identity() +
    ggfittext::geom_fit_text(
      data      = pos_data,
      aes(label = label_txt),
      place     = "right",
      grow      = TRUE,
      min.size  = 3.0,
      outside   = TRUE,
      padding.x = unit(1.2, "mm"),
      contrast  = TRUE,
      color     = "#2B3555",
      fontface  = "bold"
    ) +
    ggfittext::geom_fit_text(
      data      = neg_data,
      aes(label = label_txt),
      place     = "left",
      grow      = TRUE,
      min.size  = 3.0,
      outside   = TRUE,
      padding.x = unit(1.2, "mm"),
      contrast  = TRUE,
      color     = "#2B3555",
      fontface  = "bold"
    ) +
    geom_hline(yintercept = 0, color = "#000000", linetype = "solid", linewidth = 0.5) +
    scale_y_continuous(
      limits = c(y_min_lim, y_max_lim),
      breaks = scales::breaks_width(5),
      minor_breaks = scales::breaks_width(1),
      labels = scales::label_percent(scale = 1)
    ) +
    labs(title = title_txt, subtitle = subtitle_txt, x = NULL, y = NULL) +
    theme_classic() +
    theme(
      text = element_text(family = "Verdana"),
      plot.title = element_text(hjust = 0.5, color = "#2B3555", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555", size = 10),
      axis.text.y = element_text(hjust = 1, color = "#2B3555"),
      axis.text.x = element_text(color = "#2B3555"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.25),
      axis.line.y = element_line(color = "black", size = 0.25),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", linetype = "solid", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 80, 20, 20)
    ) +
    coord_flip(clip = "off")

  # ---- Return slide contract ----
  list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  )
}
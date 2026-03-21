# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(stringr)
library(tidyr)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "NISS Theme",
  fullsize = FALSE
) {

  # Load and filter the data
  data <- read_csv(source_file, show_col_types = FALSE) %>%
    filter(launch_year == 2022) %>%
    mutate(
      ret_2022 = as.numeric(ret_2022) / 100,
      ret_2023 = as.numeric(ret_2023) / 100
    )

  # Average retention per School for 2022 and 2023
  average_retention <- data %>%
    group_by(School) %>%
    summarise(
      ret_2022 = mean(ret_2022, na.rm = TRUE),
      ret_2023 = mean(ret_2023, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("ret_"),
      names_to = "Year",
      values_to = "AverageRetention"
    ) %>%
    mutate(Year = str_replace(Year, "ret_", ""))

  # Sort schools by 2023 retention (ascending)
  school_order <- average_retention %>%
    filter(Year == "2023") %>%
    arrange(AverageRetention) %>%
    pull(School)

  average_retention$School <- factor(average_retention$School, levels = school_order)

  # Keep schools with at least one value
  average_retention <- average_retention %>%
    group_by(School) %>%
    filter(!all(is.na(AverageRetention))) %>%
    ungroup()

  # Label placement rules (higher above, lower below; ties -> 2023 on top)
  tol <- 1e-9
  average_retention <- average_retention %>%
    mutate(Year = factor(Year, levels = c("2022", "2023"))) %>%  # for tie logic
    group_by(School) %>%
    mutate(
      n_non_na  = sum(!is.na(AverageRetention)),
      max_val   = suppressWarnings(max(AverageRetention, na.rm = TRUE)),
      min_val   = suppressWarnings(min(AverageRetention, na.rm = TRUE)),
      diff_val  = ifelse(is.finite(max_val) & is.finite(min_val), max_val - min_val, NA_real_),
      pos = case_when(
        is.na(AverageRetention) ~ NA_character_,
        n_non_na == 1 ~ "top",
        diff_val <= tol & Year == "2023" ~ "top",
        diff_val <= tol & Year == "2022" ~ "bottom",
        AverageRetention == max_val ~ "top",
        TRUE ~ "bottom"
      )
    ) %>%
    ungroup() %>%
    mutate(Year = as.character(Year))  # back to character for scale mapping

  # Label spacing
  label_nudge <- 0.025

  # Plot
  title_txt <- "Average Retention for Launch Year 2022"
  subtitle_txt <- "(2022 vs 2023)"

  plt <- ggplot(average_retention, aes(x = School, y = AverageRetention, color = Year)) +
    geom_line(aes(group = School), color = "#58595B", linewidth = 1) +
    geom_point(size = 6) +
    geom_text(
      data = dplyr::filter(average_retention, pos == "top"),
      aes(label = scales::percent(AverageRetention, accuracy = 1)),
      nudge_y = label_nudge, vjust = 0,
      size = 3, fontface = "bold",
      show.legend = FALSE
    ) +
    geom_text(
      data = dplyr::filter(average_retention, pos == "bottom"),
      aes(label = scales::percent(AverageRetention, accuracy = 1)),
      nudge_y = -label_nudge, vjust = 1,
      size = 3, fontface = "bold",
      show.legend = FALSE
    ) +
    theme_classic() +
    theme(
      plot.margin = margin(t = 30, r = 30, b = 50, l = 50),
      plot.title = element_text(hjust = 0.5, color = "#2B3555", family = "Verdana", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555", family = "Verdana", size = 10),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 8, color = "#2B3555", margin = margin(t = 5), angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12, color = "#2B3555"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 10)
    ) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(ylim = c(0.50, 1.05), clip = "off") +
    scale_y_continuous(
      breaks = seq(0.5, 1.0, by = 0.1),
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    scale_color_manual(
      values = c("2023" = "#0554A3", "2022" = "#a1a1a1"),
      breaks = c("2023", "2022")
    ) +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 4)))

  # Return slide contract
  list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  )
}
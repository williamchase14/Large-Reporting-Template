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
  # 1) Load data
  data <- read_csv(source_file, show_col_types = FALSE)

  # 2) Prep retention fields (as proportions)
  data <- data %>%
    mutate(
      ret_2022 = as.numeric(ret_2022) / 100,
      ret_2023 = as.numeric(ret_2023) / 100
    ) %>%
    mutate(
      School = if_else(
        School == "CUNY Borough of Manhattan Community College",
        "CUNY BMCC",
        School
      )
    )

  # 3) Average per school and reshape
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

  # 4) Order schools by 2023 retention (ascending) and drop all-missing rows
  school_order <- average_retention %>%
    filter(Year == "2023") %>%
    arrange(AverageRetention) %>%
    pull(School)

  average_retention$School <- factor(average_retention$School, levels = school_order)

  average_retention <- average_retention %>%
    group_by(School) %>%
    filter(!all(is.na(AverageRetention))) %>%
    ungroup()

  # 5) Label placement: higher above, lower below; ties -> 2023 above, 2022 below
  label_nudge <- 0.025

  average_retention <- average_retention %>%
    group_by(School) %>%
    mutate(
      max_val = max(AverageRetention, na.rm = TRUE),
      min_val = min(AverageRetention, na.rm = TRUE),
      tie = dplyr::near(max_val, min_val, tol = 1e-9),
      sign = dplyr::case_when(
        sum(!is.na(AverageRetention)) == 1 ~ 1,
        tie & Year == "2023" ~ 1,
        tie & Year == "2022" ~ -1,
        AverageRetention == max_val ~ 1,
        AverageRetention == min_val ~ -1,
        TRUE ~ 1
      ),
      nudge_y = sign * label_nudge,
      vjust_lab = if_else(sign == 1, 0, 1)
    ) %>%
    ungroup()

  # 6) Build plot
  title_txt <- "Average Retention by School"
  subtitle_txt <- "(2022 vs 2023)"

  plt <- ggplot(average_retention, aes(x = School, y = AverageRetention, color = Year)) +
    geom_line(aes(group = School), color = "#58595B", linewidth = 1) +
    geom_point(size = 6) +
    geom_text(
      aes(
        label = scales::percent(AverageRetention, accuracy = 1),
        y = AverageRetention + nudge_y,
        vjust = vjust_lab
      ),
      size = 3, fontface = "bold", check_overlap = TRUE
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
      axis.text.y = element_text(size = 10, color = "#2B3555"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 10)
    ) +
    scale_y_continuous(
      labels = label_percent(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.06))
    ) +
    coord_cartesian(ylim = c(0.4, 1.05), clip = "off") +
    scale_color_manual(values = c("2023" = "#0554A3", "2022" = "#a1a1a1")) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    labs(x = NULL, y = NULL)

  # 7) Return slide contract
  list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  )
}

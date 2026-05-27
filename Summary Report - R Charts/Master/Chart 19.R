# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(stringr)
library(ggtext)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "Theme",
  fullsize = FALSE
) {
  # Load and prepare the data
  data <- read_csv(source_file, show_col_types = FALSE) %>%
    mutate(`Weighted % Complete` = na_if(`Weighted % Complete`, "No Valid Responses")) %>%
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # Keep only the MOST RECENT survey for each school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # Normalize pillar names (decode multi-escaped '&amp;' to plain '&amp;')
  norm <- latest_by_school
  for (i in 1:4) {
    norm <- norm %>% mutate(Pillar = str_replace_all(Pillar, "&amp;amp;", "&amp;"))
  }
  norm <- norm %>% mutate(Pillar = str_trim(Pillar))

  # Canonical pillar labels (plain '&amp;')
  selected_pillars <- c(
    "Career Oriented Learning",
    "Data",
    "Financial Wellness",
    "Proactive Advising",
    "Academic Design &amp; Support",
    "Outreach and Communication",
    "Structured First Year Support"
  )

  # Filter to launch year 2024 + selected pillars
  filtered_data <- norm %>%
    filter(Pillar %in% selected_pillars, launch_year == 2024)

  # Per-school, per-pillar averages
  summary_table <- filtered_data %>%
    group_by(School, Pillar) %>%
    summarise(
      Average_Weighted_Complete = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Average_Weighted_Complete = na_if(Average_Weighted_Complete, NaN))

  # Grand Total row
  grand_total <- summary_table %>%
    group_by(Pillar) %>%
    summarise(
      Average_Weighted_Complete = mean(Average_Weighted_Complete, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(School = "<b>Grand Total</b>") %>%
    relocate(School, Pillar)

  # Order: Z -> A with Grand Total at the bottom
  school_levels <- sort(unique(summary_table$School), decreasing = TRUE)
  y_levels <- c("<b>Grand Total</b>", school_levels)

  summary_with_total <- bind_rows(summary_table, grand_total) %>%
    mutate(School = factor(School, levels = y_levels))

  # Dynamic institution count for subtitle
  num_schools <- length(school_levels)

  # Gridline positions
  nx <- n_distinct(summary_with_total$Pillar)
  ny <- n_distinct(summary_with_total$School)
  x_mid <- if (nx > 1) seq(1.5, nx - 0.5, by = 1) else numeric(0)
  y_mid <- if (ny > 1) seq(1.5, ny - 0.5, by = 1) else numeric(0)

  wrap_width <- 16

  # Plot
  title_txt <- "Progress Scores by Pillar"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  plt <- ggplot(summary_with_total, aes(x = Pillar, y = School)) +
    # white band behind Grand Total row
    geom_tile(
      data = dplyr::filter(summary_with_total, School == "<b>Grand Total</b>"),
      fill = "white", linewidth = 0
    ) +
    geom_tile(
      data = dplyr::filter(summary_with_total, School != "<b>Grand Total</b>"),
      aes(fill = Average_Weighted_Complete),
      linewidth = 0
    ) +
    geom_vline(xintercept = x_mid, color = "grey85", linewidth = 0.4) +
    geom_hline(yintercept = y_mid, color = "grey85", linewidth = 0.4) +
    geom_hline(yintercept = 1.5, color = "#00bfff", linewidth = 0.9) +
    geom_text(
      aes(label = scales::percent(Average_Weighted_Complete, accuracy = 1)),
      size = 3, fontface = "bold", family = "Verdana",
      hjust = 0.5, vjust = 0.5, color = "#2B3555"
    ) +
    scale_fill_gradientn(
      colors = c("#8B0000", "#F7F7F7", "#0B6E28"),
      values = scales::rescale(c(0, 0.52, 1)),
      limits = c(0, 1),
      na.value = "grey95",
      labels = scales::percent_format(accuracy = 1),
      name = "% Complete"
    ) +
    scale_y_discrete(limits = y_levels, expand = c(0, 0)) +
    scale_x_discrete(
      position = "top",
      expand = c(0, 0),
      labels = function(x) stringr::str_wrap(x, width = wrap_width)
    ) +
    theme_minimal(base_family = "Verdana") +
    theme(
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text.y = ggtext::element_markdown(
        hjust = 1, color = "#2B3555", size = 10, margin = margin(r = 6)
      ),
      axis.text.x = element_text(
        hjust = 0.5, vjust = 1, size = 9, color = "#2B3555",
        lineheight = 0.95, margin = margin(b = 6), face = "bold"
      ),
      plot.title = element_text(color = "#2B3555", size = 16, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555", size = 10),
      legend.position = "none",
      plot.margin = margin(10, 60, 10, 40)
    ) +
    coord_cartesian(clip = "off") +
    annotate(
      "segment", x = 0.5, xend = nx + 0.5, y = ny + 0.5, yend = ny + 0.5,
      color = "#2B3555", size = 0.9
    ) +
    labs(
      title = title_txt,
      subtitle = subtitle_txt,
      x = NULL, y = NULL
    )

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
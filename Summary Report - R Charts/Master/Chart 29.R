# ---- Libraries ----
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(scales)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "NISS Theme",
  fullsize = FALSE
) {
  # 1) Load data
  data <- read_csv(source_file, show_col_types = FALSE)

  # 2) Prepare fields and normalize text
  data <- data %>%
    mutate(
      `Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`)),
      survey_num = readr::parse_number(`Survey #`)
    ) %>%
    # Decode multi-escaped ampersands
    mutate(
      engagement_type = gsub("&amp;amp;", "&amp;", engagement_type, fixed = TRUE),
      Pillar          = gsub("&amp;amp;", "&amp;", Pillar,          fixed = TRUE)
    ) %>%
    mutate(
      engagement_type = gsub("&amp;", "&", engagement_type, fixed = TRUE),
      Pillar          = gsub("&amp;", "&", Pillar,          fixed = TRUE)
    )

  # 3) Most recent survey per school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Filter to selected pillars AND engagement type (Diagnostic only)
  selected_pillars <- c(
    "Academic Design & Support",
    "Career Oriented Learning",
    "Data",
    "Financial Wellness",
    "Proactive Advising",
    "Outreach and Communication",
    "Structured First Year Support"
  )

  filtered_data <- latest_by_school %>%
    filter(
      engagement_type == "1. Diagnostic only",
      Pillar %in% selected_pillars
    )

  # 5) Group by Pillar and compute the average (0–1 scale)
  pillar_averages <- filtered_data %>%
    group_by(Pillar) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    )

  # Order Pillars alphabetically (A–Z), then reversed for coord_flip layout
  pillar_averages$Pillar <- factor(
    pillar_averages$Pillar,
    levels = sort(unique(pillar_averages$Pillar), decreasing = TRUE)
  )

  num_schools <- dplyr::n_distinct(filtered_data$School)

  # 6) Build chart
  title_txt <- "Diagnostic Only – Average by Pillar"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  plt <- ggplot(pillar_averages, aes(x = Pillar, y = Average_Weighted_Completed)) +
    geom_bar(stat = "identity", fill = "#2B3555", width = 0.5) +
    geom_text(aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
              hjust = 1.1,
              color = "#ffffff", fontface = "bold", size = 4) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(hjust = 1, color = "#2B3555", family = "Verdana", size = 8),
      axis.text.y = element_text(hjust = 1, color = "#2B3555", family = "Verdana", size = 8),
      legend.position = "none",
      axis.line.x = element_line(color = "black", size = .25),
      axis.line.y = element_line(color = "black", size = .25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    coord_flip()

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
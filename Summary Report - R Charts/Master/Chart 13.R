# ---- Libraries ----
library(ggplot2)
library(readxl)
library(dplyr)
library(scales)
library(readr)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "Theme",
  fullsize = FALSE
) {

  # 1) Load data
  data <- read_csv(source_file, show_col_types = FALSE)

  # 2) Prepare fields
  data <- data %>%
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # 3) Most recent survey per school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Filter to specified pillars (include label variants)
  filtered_data <- latest_by_school %>%
    filter(Pillar %in% c(
      "Academic Design & Support",
      "Academic Design &amp; Support",
      "Academic Design &amp;amp; Support",
      "Career Oriented Learning",
      "Data",
      "Financial Wellness",
      "Proactive Advising",
      "Outreach and Communication",
      "Outreach and Communications",
      "Structured First Year Support"
    ))

  # Factor launch_year
  filtered_data$launch_year <- as.factor(filtered_data$launch_year)

  # 5) Compute averages + subtitle
  pillar_year_averages <- filtered_data %>%
    group_by(Pillar, launch_year) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    )

  num_institutions <- filtered_data %>% distinct(School) %>% nrow()

  # 6) Build chart
  title_txt <- "Progress Score Across Pillars by Launch Year"
  subtitle_txt <- paste0("(", num_institutions, " Institutions)")

  plt <- ggplot(
    pillar_year_averages,
    aes(x = Pillar, y = Average_Weighted_Completed, fill = factor(launch_year))
  ) +
    geom_bar(stat = "identity",
             position = position_dodge(width = 0.8),
             width = 0.79) +
    geom_text(aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
              position = position_dodge(width = 0.8),
              hjust = -0.1,
              color = "#2B3555", fontface = "bold", size = 4) +
    scale_y_continuous(labels = percent_format(scale = 100),
                       limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_manual(values = c("2024" = "#b8b8b8", "2023" = "#E03244", "2022" = "#0554A3")) +
    guides(fill = guide_legend(override.aes = list(size = 4))) +
    labs(title = title_txt, subtitle = subtitle_txt, x = NULL, y = NULL, fill = element_blank()) +
    theme_minimal() +
    theme(
      plot.title    = element_text(hjust = 0.5, color = "#2B3555", family = "Verdana", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555", family = "Verdana", size = 10),
      axis.text.x   = element_text(color = "#2B3555", family = "Verdana", size = 12),
      axis.text.y   = element_text(color = "#2B3555", family = "Verdana", size = 12),
      axis.line     = element_line(color = "black", size = 0.25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border  = element_blank(),
      legend.position = "right",
      legend.title  = element_text(color = "#2B3555", family = "Verdana", size = 10),
      legend.text   = element_text(color = "#2B3555", family = "Verdana", size = 10),
      legend.key.size = unit(0.1, "cm"),
      plot.margin   = margin(10, 60, 10, 10)
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
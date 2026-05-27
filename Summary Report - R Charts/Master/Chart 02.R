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

  # 2) Prep fields
  data <- data %>%
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # 3) Most recent survey per school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Pillars of interest
  filtered_data <- latest_by_school %>%
    filter(Pillar %in% c(
      "Academic Design & Support",
      "Career Oriented Learning",
      "Data",
      "Financial Wellness",
      "Proactive Advising",
      "Outreach and Communication",
      "Structured First Year Support"
    ))

  # 5) Compute pillar averages
  school_pillar <- filtered_data %>%
    group_by(School, Pillar) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    )

  pillar_averages <- school_pillar %>%
    group_by(Pillar) %>%
    summarise(
      Average_Weighted_Completed = mean(Average_Weighted_Completed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Average_Weighted_Completed)

  num_schools <- length(unique(filtered_data$School))

  title_txt <- "Progress Score Across Pillars"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  # 6) Chart
  plt <- ggplot(pillar_averages,
                aes(x = reorder(Pillar, Average_Weighted_Completed),
                    y = Average_Weighted_Completed)) +
    geom_bar(stat = "identity", fill = "#0554A3", width = 0.5) +
    geom_text(aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
              hjust = 1.1,
              size = 4,
              color = "white",
              fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1),
                       expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "#2B3555",
                                family = "Verdana", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555",
                                   family = "Verdana", size = 10),
      axis.text.x = element_text(hjust = 1, color = "#2B3555",
                                 family = "Verdana", size = 12),
      axis.text.y = element_text(hjust = 1, color = "#2B3555",
                                 family = "Verdana", size = 12),
      legend.position = "none",
      axis.line.x = element_line(color = "black", size = .25),
      axis.line.y = element_line(color = "black", size = .25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    coord_flip()

  # 7) Return the contract
  return(list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  ))
}

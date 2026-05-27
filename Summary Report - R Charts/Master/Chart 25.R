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
  master = "Theme",
  fullsize = FALSE
) {
  # 1) Load data
  data <- read_csv(source_file, show_col_types = FALSE)

  # 2) Prepare fields
  data <- data %>%
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # (Optional legacy rename was incorrect in original; no-op here)

  # 3) Most recent survey per school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Filter to "1. Diagnostic only"
  filtered_data <- latest_by_school %>%
    filter(engagement_type == "1. Diagnostic only")

  # 5) Aggregate + labels
  average_weighted_completed <- mean(filtered_data$`Weighted % Complete`, na.rm = TRUE) * 100
  num_schools <- dplyr::n_distinct(filtered_data$School)

  title_txt <- "Diagnostic Only"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  plot_data <- data.frame(Category = "Average", Value = average_weighted_completed)

  # 6) Build plot
  plt <- ggplot(plot_data, aes(x = Category, y = Value)) +
    geom_bar(stat = "identity", fill = "#2B3555", width = 0.5) +
    geom_text(aes(label = paste0(round(Value, 0), "%")),
              vjust = 0.5, hjust = 1.3,
              color = "white", size = 4, fontface = "bold") +
    scale_y_continuous(limits = c(0, 100),
                       labels = scales::percent_format(scale = 1),
                       expand = c(0, 0)) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      text = element_text(family = "Verdana"),
      plot.title = element_text(size = 12, family = "Verdana", face = "bold",
                                color = "#2B3555", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555",
                                   family = "Verdana", size = 10),
      axis.text.y = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(color = "#2B3555", family = "Verdana", size = 10),
      axis.line.x = element_line(color = "black", size = 0.25),
      plot.margin = margin(t = 10, r = 40, b = 10, l = 10)
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
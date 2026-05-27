# ---- Libraries ----
library(ggplot2)
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

  # 2) Prepare fields for latest-survey logic
  data <- data %>%
    mutate(
      `Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`)),
      survey_num = readr::parse_number(`Survey #`)
    )

  # 3) Keep only the MOST RECENT survey for each school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Clean MSI Type, then filter to non-empty values
  latest_by_school <- latest_by_school %>%
    mutate(
      `MSI Type` = trimws(`MSI Type`),
      `MSI Type` = if_else(`MSI Type` == "#N/A", "Non-MSI", `MSI Type`)
    )

  filtered_data <- latest_by_school %>%
    filter(!is.na(`MSI Type`) & `MSI Type` != "")

  # 5) Group by MSI Type: average score + distinct school count
  msi_averages <- filtered_data %>%
    group_by(`MSI Type`) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      Schools_in_MSI = n_distinct(School),
      .groups = "drop"
    ) %>%
    mutate(MSI_Label = paste0(`MSI Type`, " (", Schools_in_MSI, ")")) %>%
    arrange(Average_Weighted_Completed)

  num_schools <- dplyr::n_distinct(filtered_data$School)

  # 6) Build horizontal bar chart
  title_txt <- "Progress Score Across MSI Types"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  plt <- ggplot(
    msi_averages,
    aes(x = reorder(MSI_Label, Average_Weighted_Completed),
        y = Average_Weighted_Completed)
  ) +
    geom_bar(stat = "identity", fill = "#0554A3", width = 0.5) +
    geom_text(aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
              hjust = 1.1, size = 4, color = "#ffffff", fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title    = element_text(hjust = 0.5, color = "#2B3555", family = "Verdana", size = 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555", family = "Verdana", size = 10),
      axis.text.x   = element_text(hjust = 1, color = "#2B3555", family = "Verdana", size = 10),
      axis.text.y   = element_text(hjust = 1, color = "#2B3555", family = "Verdana", size = 10),
      legend.position = "none",
      axis.line.x   = element_line(color = "black", size = .25),
      axis.line.y   = element_line(color = "black", size = .25),
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
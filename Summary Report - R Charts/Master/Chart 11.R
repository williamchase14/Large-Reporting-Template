# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(stringr)

# ---- Function: build_slide() ----
# Returns: list(plot, title, subtitle, layout, master, fullsize)
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
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # 3) Keep only the MOST RECENT survey for each school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Keep all pillars; convert launch_year to factor
  latest_by_school$launch_year <- as.factor(latest_by_school$launch_year)

  # 5) Average "Weighted % Complete" by launch_year
  average_data <- latest_by_school %>%
    group_by(launch_year) %>%
    summarise(
      average_weighted_completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    )

  # 6) Build plot (no file saving)
  title_txt <- "Average Weighted % Complete by Launch Year"
  subtitle_txt <- ""

  plt <- ggplot(
    average_data,
    aes(x = launch_year, y = average_weighted_completed, fill = launch_year)
  ) +
    geom_col(width = 0.6) +
    geom_text(
      aes(label = scales::percent(average_weighted_completed, accuracy = 1)),
      vjust = -0.5, color = "#2B3555", size = 5, fontface = "bold"
    ) +
    theme_classic() +
    theme(
      plot.title   = element_text(size = 16, color = "#2B3555", face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(size = 12, color = "#2B3555"),
      axis.text.y  = element_text(size = 12, color = "#2B3555"),
      legend.position = "none"
    ) +
    labs(title = title_txt) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(), expand = c(0, 0)) +
    scale_fill_manual(values = c("2024" = "#b8b8b8", "2023" = "#E03244", "2022" = "#0554A3"))

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
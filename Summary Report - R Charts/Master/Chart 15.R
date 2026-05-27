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

  # 2) Clean + prepare for latest-survey logic
  data <- data %>%
    mutate(`Weighted % Complete` = na_if(`Weighted % Complete`, "No Valid Responses")) %>%
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # 3) Most recent survey per school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Average progress per school + store launch year
  Progress_Ave <- latest_by_school %>%
    group_by(School) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      Year = dplyr::first(launch_year),
      .groups = "drop"
    ) %>%
    filter(!is.na(Average_Weighted_Completed)) %>%
    arrange(desc(Average_Weighted_Completed))

  num_schools <- dplyr::n_distinct(Progress_Ave$School)

  # 5) Build plot
  title_txt <- "Progress Score by Launch Year"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  plt <- ggplot(
    Progress_Ave,
    aes(
      x = reorder(School, Average_Weighted_Completed),
      y = Average_Weighted_Completed,
      fill = as.factor(Year)
    )
  ) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(
      aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
      hjust = -0.2, size = 3, color = "#2B3555", fontface = "bold"
    ) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_manual(
      values = c("#0554A3", "#E03244", "#919191"),
      name = "Year"
    ) +
    labs(x = NULL, y = NULL) +
    theme_classic() +
    theme(
      text = element_text(family = "Verdana"),
      plot.title =
        element_text(hjust = 0.5, color = "#2B3555", size = 16, face = "bold"),
      plot.subtitle =
        element_text(hjust = 0.5, color = "#2B3555", size = 10),
      axis.text.y = element_text(hjust = 1, color = "#2B3555"),
      axis.text.x = element_text(color = "#2B3555"),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.text = element_text(color = "#2B3555"),
      legend.title = element_blank(),
      legend.position = "right",
      legend.key.size = grid::unit(0.5, "lines"),
      axis.line.x = element_line(color = "black", size = 0.25),
      axis.line.y = element_line(color = "black", size = 0.25),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 60, 20, 20)
    ) +
    coord_flip()

  # 6) Return slide contract
  list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  )
}
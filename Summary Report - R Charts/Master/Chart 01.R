# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

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

  # 2) Prepare fields
  data <- data %>%
    mutate(`Weighted % Complete` = suppressWarnings(as.numeric(`Weighted % Complete`))) %>%
    mutate(survey_num = readr::parse_number(`Survey #`))

  # 3) Keep only the most recent survey per school
  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # 4) Single score per school
  school_scores <- latest_by_school %>%
    filter(!is.na(`Weighted % Complete`)) %>%
    group_by(School) %>%
    summarise(
      school_weighted_pct = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    )

  # 5) Aggregate + labels
  average_weighted_completed <- mean(school_scores$school_weighted_pct, na.rm = TRUE) * 100
  num_schools <- dplyr::n_distinct(school_scores$School)

  title_txt <- "Progress Score Across All Institutions"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  # 6) Plot (no file saving)
  plot_data <- data.frame(
    Category = "Average",
    Value = average_weighted_completed
  )

  plt <- ggplot(plot_data, aes(x = Category, y = Value)) +
    geom_bar(stat = "identity", fill = "#0554A3", width = 0.5) +
    geom_text(aes(label = paste0(round(Value, 0), "%")),
              vjust = 0.5, hjust = 1.3, color = "white", size = 6, fontface = "bold") +
    scale_y_continuous(limits = c(0, 100),
                       labels = scales::percent_format(scale = 1),
                       expand = c(0, 0)) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    labs(x = NULL, y = NULL) +  # titles handled separately
    theme_minimal() +
    theme(
      text = element_text(family = "Verdana"),
      axis.text.y = element_blank(),
      axis.line.y = element_line(color = "black", size = 0.25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(color = "#2B3555", family = "Verdana", size = 16),
      axis.line.x = element_line(color = "black", size = 0.25),
      plot.margin = margin(t = 10, r = 40, b = 10, l = 10)
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

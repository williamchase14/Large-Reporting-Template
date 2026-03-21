# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)

# ---- Function: build_slide() ----
# Returns a list: plot, title, subtitle, layout, master, fullsize
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "NISS Theme",
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

  # 4) Filter to Proactive Advising and summarize
  proactive_advising_data <- latest_by_school %>%
    filter(Pillar == "Proactive Advising") %>%
    group_by(School) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Average_Weighted_Completed))

  num_schools <- dplyr::n_distinct(proactive_advising_data$School)

  title_txt <- "Progress Score - Proactive Advising"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  # 5) Build plot (no file saving)
  plt <- ggplot(
    proactive_advising_data,
    aes(x = reorder(School, Average_Weighted_Completed),
        y = Average_Weighted_Completed)
  ) +
    geom_bar(stat = "identity", fill = "#0554A3", width = 0.75) +
    geom_text(aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
              hjust = -0.2, size = 3, color = "#2B3555",
              family = "Verdana", fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, color = "#2B3555",
                                family = "Verdana", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555",
                                   family = "Verdana", size = 10),
      axis.text.y = element_text(hjust = 1, color = "#2B3555", family = "Verdana", size = 9),
      axis.text.x = element_text(color = "#2B3555", family = "Verdana", size = 10),
      legend.position = "none",
      axis.line.x = element_line(color = "black", size = .25),
      axis.line.y = element_line(color = "black", size = .25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(20, 95, 20, 20)
    ) +
    coord_flip(clip = "off")

  # 6) Return the contract
  return(list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  ))
}
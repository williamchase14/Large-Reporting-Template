# ---- Libraries ----
library(ggplot2)
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

  # 5) Outreach & Communications per school
  outreach_communications_data <- latest_by_school %>%
    filter(Pillar == "Outreach and Communication") %>%
    group_by(School) %>%
    summarise(
      Average_Weighted_Completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Average_Weighted_Completed))

  num_schools <- dplyr::n_distinct(outreach_communications_data$School)

  title_txt <- "Progress Score - Outreach\n& Communications"
  subtitle_txt <- paste0("(", num_schools, " Institutions)")

  # 6) Plot (no file saving)
  plt <- ggplot(
    outreach_communications_data,
    aes(x = reorder(School, Average_Weighted_Completed),
        y = Average_Weighted_Completed)
  ) +
    geom_bar(stat = "identity", fill = "#0554A3", width = 0.75) +
    geom_text(aes(label = paste0(round(Average_Weighted_Completed * 100, 0), "%")),
              hjust = 1.1,
              size = 3.5,
              color = "white",
              family = "Verdana",
              fontface = "bold") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title    = element_text(hjust = 0.5, color = "#2B3555",
                                   family = "Verdana", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555",
                                   family = "Verdana", size = 10),
      axis.text.y   = element_text(hjust = 1, color = "#2B3555",
                                   family = "Verdana", size = 10),
      axis.text.x   = element_text(color = "#2B3555",
                                   family = "Verdana", size = 10),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(color = "black", size = 0.25),
      plot.margin = margin(10, 60, 10, 10)
    ) +
    coord_flip(clip = "off")

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
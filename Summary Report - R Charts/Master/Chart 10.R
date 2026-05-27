# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(scales)
library(glue)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "Theme",
  fullsize = FALSE
) {

  # 1) Load data
  data <- read_csv(source_file, show_col_types = FALSE)

  # 2) Prepare launch-year summary
  school_counts <- data %>%
    filter(!is.na(launch_year)) %>%
    group_by(launch_year) %>%
    summarise(n = n_distinct(unitid), .groups = "drop_last") %>%
    arrange(desc(launch_year)) %>%
    mutate(
      n_pretty = prettyNum(n, big.mark = ",", scientific = FALSE),
      percent = round(n / sum(n) * 100, 0),
      label = glue("Launch Year {launch_year}\n{n_pretty} Schools \n({percent}%)"),
      cumulative = cumsum(n),
      label_y_location = cumulative - n / 2
    )

  title_txt <- "Launch Year Participation \n2022 - 2024"
  subtitle_txt <- ""   # this slide has no subtitle

  # 3) Donut plot (no file saving)
  plt <- ggplot(school_counts, aes(x = 1.5, y = n, fill = as.factor(launch_year))) +
    theme_void() +
    theme(
      plot.title = element_text(
        size = 16, color = "#2B3555", face = "bold",
        hjust = 0.5, margin = margin(t = 10, b = 5),
        family = "Verdana"
      )
    ) +
    labs(title = title_txt, subtitle = NULL) +
    geom_bar(stat = "identity", color = "white", width = 0.8) +
    coord_polar(theta = "y", start = -pi / 2) +
    xlim(c(0.5, 2.5)) +
    scale_fill_manual(values = c("2022" = "#0554A3",
                                 "2023" = "#E03244",
                                 "2024" = "#b8b8b8")) +
    guides(fill = "none", color = "none") +
    geom_label(
      aes(x = 2.4, y = label_y_location, label = label, color = as.factor(launch_year)),
      fontface = "bold",
      size = 3,
      fill = NA,
      label.size = NA,
      family = "Verdana"
    ) +
    scale_color_manual(values = c("2022" = "#0554A3",
                                  "2023" = "#E03244",
                                  "2024" = "#9e9e9e"))

  # 4) Return the contract
  return(list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  ))
}
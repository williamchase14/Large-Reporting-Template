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
    filter(!all(is.na(survey_num))) %>%                     # drop schools with no survey number
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>% # keep rows at the max per school (ties kept)
    ungroup()

  # 4) Pillar filtering (normalize '&amp;' -> '&' and include both Outreach variants)
  latest_by_school <- latest_by_school %>%
    mutate(Pillar = str_replace_all(Pillar, "&amp;", "&"))

  pillars_to_include <- c(
    "Data",
    "Career Oriented Learning",
    "Financial Wellness",
    "Proactive Advising",
    "Structured First Year Support",
    "Academic Design & Support",
    "Outreach and Communication",   # singular
    "Outreach and Communications"   # plural
  )

  filtered_data <- latest_by_school %>%
    filter(Pillar %in% pillars_to_include)

  # Convert launch_year to factor
  filtered_data$launch_year <- as.factor(filtered_data$launch_year)

  # 5) Average "Weighted % Complete" by Pillar & launch_year
  average_data <- filtered_data %>%
    group_by(Pillar, launch_year) %>%
    summarise(
      average_weighted_completed = mean(`Weighted % Complete`, na.rm = TRUE),
      .groups = "drop"
    )

  # Wrap the text for the X-axis labels
  average_data$Pillar <- str_wrap(average_data$Pillar, width = 10)

  # 6) Build plot (no file saving)
  title_txt    <- "Launch Year 2022–2024: Progress by Pillar"
  subtitle_txt <- ""

  plt <- ggplot(
    average_data,
    aes(x = Pillar, y = average_weighted_completed, color = launch_year)
  ) +
    geom_line(aes(group = Pillar), color = "#58595B", linewidth = 1) +
    geom_point(size = 10) +
    geom_text(
      aes(label = scales::percent(average_weighted_completed, accuracy = 1)),
      color = "white", size = 3, vjust = 0.5, fontface = "bold"
    ) +
    theme_classic() +
    theme(
      plot.title   = element_text(size = 16, color = "#2B3555", face = "bold",
                                  hjust = 0.5, margin = margin(t = 10, b = 5)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x  = element_text(size = 8, color = "#2B3555", margin = margin(t = 5)),
      axis.text.y  = element_text(size = 15, color = "#2B3555"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.text  = element_text(size = 10)
    ) +
    labs(title = title_txt) +
    scale_y_continuous(
      limits = c(0.30, 0.80),
      labels = scales::percent_format()
    ) +
    scale_color_manual(values = c("2024" = "#b8b8b8", "2023" = "#E03244", "2022" = "#0554A3")) +
    guides(color = guide_legend(override.aes = list(size = 4))) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

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
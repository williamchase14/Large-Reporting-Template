# ---- Libraries ----
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

# ---- Function: build_slide() ----
build_slide <- function(
  source_file = "00 Source data file.csv",
  layout = "Content Only",
  master = "NISS Theme",
  fullsize = FALSE
) {
  options(scipen = 999)

  # -------------------------
  # Config: pillar order
  # -------------------------
  pillars <- c(
    "Academic Design & Support",
    "Career Oriented Learning",
    "Data",
    "Financial Wellness",
    "Proactive Advising",
    "Outreach and Communication",
    "Structured First Year Support"
  )

  # -------------------------
  # Load & prep
  # -------------------------
  data <- read_csv(source_file, show_col_types = FALSE)

  data$`Weighted % Complete` <- suppressWarnings(as.numeric(data$`Weighted % Complete`))

  # Clean MSI Type
  data <- data %>%
    mutate(
      `MSI Type` = trimws(`MSI Type`),
      `MSI Type` = if_else(`MSI Type` == "#N/A", "Non-MSI", `MSI Type`)
    )

  # Latest-survey logic
  data <- data %>% mutate(survey_num = readr::parse_number(`Survey #`))

  latest_by_school <- data %>%
    group_by(School) %>%
    filter(!all(is.na(survey_num))) %>%
    filter(survey_num == max(survey_num, na.rm = TRUE)) %>%
    ungroup()

  # Filter to pillars and enforce pillar order
  filtered_data <- latest_by_school %>%
    filter(Pillar %in% pillars) %>%
    mutate(Pillar = factor(Pillar, levels = pillars))

  # Distinct school counts per MSI Type
  msi_counts <- filtered_data %>%
    group_by(`MSI Type`) %>%
    summarise(n_schools = n_distinct(School), .groups = "drop")

  # 1) Base table: means by MSI Type x Pillar
  means_wide <- filtered_data %>%
    group_by(`MSI Type`, Pillar) %>%
    summarise(val = mean(`Weighted % Complete`, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Pillar, values_from = val) %>%
    select(`MSI Type`, all_of(pillars))

  # 2) Scale to percent if needed
  scale_to_pct_if_needed <- function(df, cols) {
    flat <- as.numeric(unlist(df[cols]))
    mx <- suppressWarnings(max(flat, na.rm = TRUE))
    if (is.finite(mx) && mx <= 1.01) df %>% mutate(across(all_of(cols), ~ . * 100)) else df
  }
  means_wide <- scale_to_pct_if_needed(means_wide, pillars)

  # 3) Round to one decimal
  means_1 <- means_wide %>% mutate(across(all_of(pillars), ~ round(., 1)))

  # 4) Add row-wise averages
  with_row_avgs <- means_1 %>%
    rowwise() %>%
    mutate(Averages = {
      m <- mean(c_across(all_of(pillars)), na.rm = TRUE)
      if (is.nan(m)) NA_real_ else round(m, 1)
    }) %>%
    ungroup()

  # 5) Bottom averages row
  avg_row <- means_1 %>%
    summarise(across(all_of(pillars), ~ round(mean(., na.rm = TRUE), 1))) %>%
    mutate(`MSI Type` = "Averages") %>%
    select(`MSI Type`, all_of(pillars))

  overall_mean <- round(mean(as.numeric(unlist(means_1[pillars])), na.rm = TRUE), 1)
  avg_row$Averages <- overall_mean

  # 6) Combine (Averages last)
  final_numeric <- bind_rows(
    with_row_avgs,
    avg_row
  )

  # 7) Append school counts (skip for the Averages row)
  final_labeled <- final_numeric %>%
    left_join(msi_counts, by = "MSI Type") %>%
    mutate(`MSI Type` = ifelse(
      is.na(n_schools), `MSI Type`, paste0(`MSI Type`, " (", n_schools, ")")
    )) %>%
    select(-n_schools)

  # ---- Build heatmap-style plot ----
  long_plot <- final_labeled %>%
    pivot_longer(
      cols = c(all_of(pillars), "Averages"),
      names_to = "Pillar",
      values_to = "value"
    )

  # Keep current order; ensure "Averages" is last on x
  x_levels <- c(pillars, "Averages")
  long_plot$Pillar <- factor(long_plot$Pillar, levels = x_levels)

  # Y levels as they appear (Averages row appears at bottom already)
  y_levels <- final_labeled$`MSI Type`
  long_plot$`MSI Type` <- factor(long_plot$`MSI Type`, levels = y_levels)

  title_txt <- "Pillar Weighted % Complete by MSI Type"
  subtitle_txt <- "Rightmost column shows averages across pillars"

  plt <- ggplot(long_plot, aes(x = Pillar, y = `MSI Type`, fill = value)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(
      aes(label = ifelse(is.na(value), "", sprintf("%.1f%%", value))),
      size = 3, color = "#2B3555", fontface = "bold"
    ) +
    scale_fill_gradient(
      low = "#F7F7F7", high = "#0554A3",
      limits = c(0, 100), na.value = "grey95",
      guide = guide_colorbar(barheight = unit(4, "cm")),
      name = NULL,
      labels = function(x) paste0(x, "%")
    ) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_minimal(base_family = "Verdana") +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(color = "#2B3555", size = 9, face = "bold"),
      axis.text.y = element_text(color = "#2B3555", size = 10),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, color = "#2B3555", size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "#2B3555", size = 10),
      plot.margin = margin(10, 40, 10, 40)
    ) +
    labs(title = title_txt, subtitle = subtitle_txt)

  # ---- Return slide contract ----
  list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  )
}
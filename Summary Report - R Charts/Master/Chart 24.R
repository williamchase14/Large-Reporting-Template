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
  master = "Theme",
  fullsize = FALSE
) {
  options(scipen = 999)

  # -------------------------
  # Config: pillar order and row order
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

  row_order_vec <- c(
    "Diagnostic only",
    "Diagnostic & Implementation",
    "Diagnostic, Implementation, & Acceleration Grant"
  )

  eng_row_order <- function(x) dplyr::case_when(
    x == row_order_vec[1] ~ 1L,
    x == row_order_vec[2] ~ 2L,
    x == row_order_vec[3] ~ 3L,
    TRUE ~ 4L
  )

  # -------------------------
  # Load & prep
  # -------------------------
  data <- read_csv(source_file, show_col_types = FALSE)

  # Convert to numeric safely
  data$`Weighted % Complete` <- suppressWarnings(as.numeric(data$`Weighted % Complete`))

  # Normalize Engagement Type names (handle numbered + HTML-escaped variants)
  data <- data %>%
    mutate(engagement_type = recode(engagement_type,
      "1. Diagnostic only" = "Diagnostic only",
      "2. Diagnostic & Implementation" = "Diagnostic & Implementation",
      "2. Diagnostic &amp; Implementation" = "Diagnostic & Implementation",
      "3. Diagnostic, Implementation, & Acceleration Grant" = "Diagnostic, Implementation, & Acceleration Grant",
      "3. Diagnostic, Implementation, &amp; Acceleration Grant" = "Diagnostic, Implementation, & Acceleration Grant",
      .default = engagement_type
    ))

  # Filter to pillars and enforce pillar column order
  filtered_data <- data %>%
    filter(Pillar %in% pillars) %>%
    mutate(Pillar = factor(Pillar, levels = pillars))

  # Distinct school counts per Engagement Type (based on filtered data)
  eng_counts <- filtered_data %>%
    group_by(engagement_type) %>%
    summarise(n_schools = n_distinct(School), .groups = "drop")

  # 1) Base table: means by Engagement Type x Pillar (raw scale)
  means_wide <- filtered_data %>%
    group_by(engagement_type, Pillar) %>%
    summarise(val = mean(`Weighted % Complete`, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Pillar, values_from = val) %>%
    select(engagement_type, all_of(pillars))

  # 2) If values are in 0–1, scale to percent (0–100). Otherwise leave as-is.
  scale_to_pct_if_needed <- function(df, cols) {
    flat <- as.numeric(unlist(df[cols]))
    mx <- suppressWarnings(max(flat, na.rm = TRUE))
    if (is.finite(mx) && mx <= 1.01) {
      df %>% mutate(across(all_of(cols), ~ . * 100))
    } else df
  }
  means_wide <- scale_to_pct_if_needed(means_wide, pillars)

  # 3) Round pillar cells to one decimal.
  means_1 <- means_wide %>%
    mutate(across(all_of(pillars), ~ round(., 1)))

  # 4) Add rightmost "Averages" column (row means from 1-decimal cells)
  with_row_avgs <- means_1 %>%
    rowwise() %>%
    mutate(Averages = {
      m <- mean(c_across(all_of(pillars)), na.rm = TRUE)
      if (is.nan(m)) NA_real_ else round(m, 1)
    }) %>%
    ungroup()

  # 5) Bottom "Averages" row = column-wise means (from 1-decimal cells)
  avg_row <- means_1 %>%
    summarise(across(all_of(pillars), ~ round(mean(., na.rm = TRUE), 1))) %>%
    mutate(engagement_type = "Averages") %>%
    select(engagement_type, all_of(pillars))

  overall_mean <- round(mean(as.numeric(unlist(means_1[pillars])), na.rm = TRUE), 1)
  avg_row$Averages <- overall_mean

  # 6) Combine body + averages row, attach row order, and sort
  final_numeric <- bind_rows(
    with_row_avgs %>% mutate(row_order = eng_row_order(engagement_type)),
    avg_row %>% mutate(row_order = 999L)
  ) %>%
    arrange(row_order, engagement_type) %>%
    select(-row_order)

  # 7) Append distinct school counts to Engagement Type labels (except Averages)
  final_labeled <- final_numeric %>%
    left_join(eng_counts, by = "engagement_type") %>%
    mutate(engagement_type = ifelse(
      is.na(n_schools), engagement_type, paste0(engagement_type, " (", n_schools, ")")
    )) %>%
    select(-n_schools)

  # -------------------------
  # Build heatmap-style plot (values in 0–100; labels show one decimal with %)
  # -------------------------
  long_plot <- final_labeled %>%
    pivot_longer(
      cols = c(all_of(pillars), "Averages"),
      names_to = "Pillar",
      values_to = "value"
    )

  # Factor y with current order to keep "Averages" last
  y_levels <- final_labeled$engagement_type
  long_plot$engagement_type <- factor(long_plot$engagement_type, levels = y_levels)

  title_txt <- "Pillar Weighted % Complete by Engagement Type"
  subtitle_txt <- "Rightmost column shows averages across pillars"

  plt <- ggplot(long_plot, aes(x = Pillar, y = engagement_type, fill = value)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = ifelse(is.na(value), "", sprintf("%.1f%%", value))),
              size = 3, color = "#2B3555", fontface = "bold") +
    scale_fill_gradient(
      low = "#F7F7F7", high = "#0554A3",
      limits = c(0, 100), na.value = "grey95",
      guide = guide_colorbar(barheight = unit(4, "cm")),
      name = NULL, labels = function(x) paste0(x, "%")
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

  # -------------------------
  # Return slide contract
  # -------------------------
  list(
    plot = plt,
    title = title_txt,
    subtitle = subtitle_txt,
    layout = layout,
    master = master,
    fullsize = fullsize
  )
}
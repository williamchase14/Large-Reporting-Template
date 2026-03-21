# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

options(scipen = 999)  # avoid scientific notation

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

# Desired row order for Engagement Types
row_order_vec <- c(
  "Diagnostic only",
  "Diagnostic & Implementation",
  "Diagnostic, Implementation, & Acceleration Grant"
)

# Helper to assign row order index (others go after the three, Averages last)
eng_row_order <- function(x) dplyr::case_when(
  x == row_order_vec[1] ~ 1L,
  x == row_order_vec[2] ~ 2L,
  x == row_order_vec[3] ~ 3L,
  TRUE ~ 4L  # any other Engagement Types (if present)
)

# -------------------------
# Load & prep
# -------------------------
data <- read_csv("00 Source data file.csv")

# Convert to numeric safely
data$`Weighted % Complete` <- suppressWarnings(as.numeric(data$`Weighted % Complete`))

# Normalize Engagement Type names (handle numbered + HTML-escaped variants)
data <- data %>%
  mutate(`engagement_type` = recode(`engagement_type`,
    "1. Diagnostic only" = "Diagnostic only",
    "2. Diagnostic & Implementation" = "Diagnostic & Implementation",
    "2. Diagnostic &amp; Implementation" = "Diagnostic & Implementation",
    "3. Diagnostic, Implementation, & Acceleration Grant" = "Diagnostic, Implementation, & Acceleration Grant",
    "3. Diagnostic, Implementation, &amp; Acceleration Grant" = "Diagnostic, Implementation, & Acceleration Grant",
    .default = `engagement_type`
  ))

# Filter to pillars and enforce pillar column order
filtered_data <- data %>%
  filter(Pillar %in% pillars) %>%
  mutate(Pillar = factor(Pillar, levels = pillars))

# -------------------------
# Distinct school counts per Engagement Type (based on filtered data)
# -------------------------
eng_counts <- filtered_data %>%
  group_by(`engagement_type`) %>%
  summarise(n_schools = n_distinct(School), .groups = "drop")

# -------------------------
# 1) Base table: means by Engagement Type x Pillar (raw scale)
# -------------------------
means_wide <- filtered_data %>%
  group_by(`engagement_type`, Pillar) %>%
  summarise(val = mean(`Weighted % Complete`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Pillar, values_from = val) %>%
  select(`engagement_type`, all_of(pillars))

# -------------------------
# 2) If values are in 0–1, scale to percent (0–100). Otherwise leave as-is.
# -------------------------
scale_to_pct_if_needed <- function(df, cols) {
  flat <- as.numeric(unlist(df[cols]))
  mx <- suppressWarnings(max(flat, na.rm = TRUE))
  if (is.finite(mx) && mx <= 1.01) {
    df %>% mutate(across(all_of(cols), ~ . * 100))
  } else df
}
means_wide <- scale_to_pct_if_needed(means_wide, pillars)

# -------------------------
# 3) Round pillar cells to *one decimal*.
#    All downstream averages will be computed from these rounded values.
# -------------------------
means_1 <- means_wide %>%
  mutate(across(all_of(pillars), ~ round(., 1)))

# -------------------------
# 4) Add rightmost "Averages" column = mean across pillars (from the 1-decimal cells)
# -------------------------
with_row_avgs <- means_1 %>%
  rowwise() %>%
  mutate(Averages = {
    m <- mean(c_across(all_of(pillars)), na.rm = TRUE)
    if (is.nan(m)) NA_real_ else round(m, 1)
  }) %>%
  ungroup()

# -------------------------
# 5) Bottom "Averages" row = column-wise means (from the 1-decimal cells)
#     - Bottom-right cell = overall mean of all displayed pillar cells
# -------------------------
avg_row <- means_1 %>%
  summarise(across(all_of(pillars), ~ round(mean(., na.rm = TRUE), 1))) %>%
  mutate(`engagement_type` = "Averages") %>%
  select(`engagement_type`, all_of(pillars))

overall_mean <- round(mean(as.numeric(unlist(means_1[pillars])), na.rm = TRUE), 1)
avg_row$Averages <- overall_mean

# -------------------------
# 6) Combine body + averages row, attach row order, and sort
# -------------------------
final_numeric <- bind_rows(
  with_row_avgs %>% mutate(row_order = eng_row_order(`engagement_type`)),
  avg_row %>% mutate(row_order = 999L)  # ensure Averages is last
) %>%
  arrange(row_order, `engagement_type`) %>%
  select(-row_order)

# -------------------------
# 7) Append distinct school counts to Engagement Type labels (except the Averages row)
# -------------------------
final_labeled <- final_numeric %>%
  left_join(eng_counts, by = "engagement_type") %>%
  mutate(`engagement_type` = ifelse(
    is.na(n_schools),
    `engagement_type`,  # "Averages" row has no count; leave as-is
    paste0(`engagement_type`, " (", n_schools, ")")
  )) %>%
  select(-n_schools)

# -------------------------
# 8) Output CSV (formatted with percent sign, e.g., "50.0%")
# -------------------------
final_formatted <- final_labeled %>%
  mutate(across(c(all_of(pillars), "Averages"),
                ~ ifelse(is.na(.), NA_character_, sprintf("%.1f%%", .))))

write_csv(final_formatted, "23 pillar_weighted_completion_by_engagement_type.csv", na = "")

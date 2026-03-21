# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

options(scipen = 999)  # avoid scientific notation

# -------------------------
# Config: pillar order and row order
# -------------------------
# Tip: If your data sometimes includes HTML-escaped ampersands (&amp;),
# you can keep these labels as-is, or normalize Pillar below if needed.
pillars <- c(
  "Academic Design & Support",
  "Career Oriented Learning",
  "Data",
  "Financial Wellness",
  "Proactive Advising",
  "Outreach and Communication",
  "Structured First Year Support"
)

# If no custom order vector is provided in the environment,
# define a harmless default so msi_row_order() works.
if (!exists("row_order_vec")) {
  row_order_vec <- c(NA_character_, NA_character_, NA_character_)
}

# Helper to assign row order index (others go after the three, Averages last)
msi_row_order <- function(x) dplyr::case_when(
  x == row_order_vec[1] ~ 1L,
  x == row_order_vec[2] ~ 2L,
  x == row_order_vec[3] ~ 3L,
  TRUE ~ 4L
)

# -------------------------
# Load & prep
# -------------------------
data <- read_csv("00 Source data file.csv", show_col_types = FALSE)

# Convert to numeric safely
data$`Weighted % Complete` <- suppressWarnings(as.numeric(data$`Weighted % Complete`))

# Normalize MSI Type names (handle numbered + HTML-escaped variants)
data <- data %>%
  mutate(
    `MSI Type` = trimws(`MSI Type`),
    `MSI Type` = if_else(`MSI Type` == "#N/A", "Non-MSI", `MSI Type`)
  )

# ---- Latest-survey logic: keep only the most recent per School ----
data <- data %>%
  mutate(survey_num = readr::parse_number(`Survey #`))

latest_by_school <- data %>%
  group_by(School) %>%
  filter(!all(is.na(survey_num))) %>%                      # drop schools with no survey number at all
  filter(survey_num == max(survey_num, na.rm = TRUE)) %>%  # keep rows at the max per school (ties kept)
  ungroup()

# Filter to pillars and enforce pillar column order
filtered_data <- latest_by_school %>%
  filter(Pillar %in% pillars) %>%
  mutate(Pillar = factor(Pillar, levels = pillars))

# -------------------------
# Distinct school counts per MSI Type
# -------------------------
msi_counts <- filtered_data %>%
  group_by(`MSI Type`) %>%
  summarise(n_schools = n_distinct(School), .groups = "drop")

# -------------------------
# 1) Base table: means by MSI Type x Pillar
# -------------------------
means_wide <- filtered_data %>%
  group_by(`MSI Type`, Pillar) %>%
  summarise(val = mean(`Weighted % Complete`, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Pillar, values_from = val) %>%
  select(`MSI Type`, all_of(pillars))

# -------------------------
# 2) Scale to percent if needed
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
# 3) Round to one decimal
# -------------------------
means_1 <- means_wide %>%
  mutate(across(all_of(pillars), ~ round(., 1)))

# -------------------------
# 4) Add row-wise averages
# -------------------------
with_row_avgs <- means_1 %>%
  rowwise() %>%
  mutate(Averages = {
    m <- mean(c_across(all_of(pillars)), na.rm = TRUE)
    if (is.nan(m)) NA_real_ else round(m, 1)
  }) %>%
  ungroup()

# -------------------------
# 5) Bottom averages row
# -------------------------
avg_row <- means_1 %>%
  summarise(across(all_of(pillars), ~ round(mean(., na.rm = TRUE), 1))) %>%
  mutate(`MSI Type` = "Averages") %>%
  select(`MSI Type`, all_of(pillars))

overall_mean <- round(mean(as.numeric(unlist(means_1[pillars])), na.rm = TRUE), 1)
avg_row$Averages <- overall_mean

# -------------------------
# 6) Combine and sort
# -------------------------
final_numeric <- bind_rows(
  with_row_avgs %>% mutate(row_order = msi_row_order(`MSI Type`)),
  avg_row %>% mutate(row_order = 999L)
) %>%
  arrange(row_order, `MSI Type`) %>%
  select(-row_order)

# -------------------------
# 7) Append school counts
# -------------------------
final_labeled <- final_numeric %>%
  left_join(msi_counts, by = "MSI Type") %>%
  mutate(`MSI Type` = ifelse(
    is.na(n_schools),
    `MSI Type`,
    paste0(`MSI Type`, " (", n_schools, ")")
  )) %>%
  select(-n_schools)

# -------------------------
# 8) Output CSV
# -------------------------
final_formatted <- final_labeled %>%
  mutate(across(c(all_of(pillars), "Averages"),
                ~ ifelse(is.na(.), NA_character_, sprintf("%.1f%%", .))))

write_csv(final_formatted, "33 pillar_weighted_completion_by_msi_type.csv", na = "")

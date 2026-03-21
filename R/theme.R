
# Centralized ggplot theme and formatting for Quarterly Summary Report

# Handles the following aspects:
    # Colors
    # Fonts
    # Text sizes
    # Grid lines
    # Legend styling
    # Overall look & feel

# Does not handle
    # Figure width / height
    # Margins
    # Orientation (horizontal vs vertical)
    # Label placement (inside / outside bars)
    # Annotations unique to that chart

# ---------------------------------------------------------
# ---------------------------------------------------------


library(ggplot2)
library(scales)

# ---- Brand colors ----
brand_colors <- list(
  navy  = "#2B3555",
  red   = "#E03244",
  blue  = "#0554A3",
  gray  = "#b8b8b8"
)

# ---- Fill scale (used for bars) ----
scale_fill_launch_year <- function() {
  scale_fill_manual(
    values = c(
      "2024" = brand_colors$red,
      "2023" = brand_colors$gray,
      "2022" = brand_colors$blue
    )
  )
}

# ---- Color scale (used for points / lines / dumbbells) ----
scale_color_launch_year <- function() {
  scale_color_manual(
    values = c(
      "2024" = brand_colors$red,
      "2023" = brand_colors$gray,
      "2022" = brand_colors$blue
    )
  )
}

# ---- Base report theme ----
theme_report <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        color = brand_colors$navy,
        family = "Verdana",
        size = 16,
        face = "bold"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        color = brand_colors$navy,
        family = "Verdana",
        size = 10
      ),
      axis.text.x = element_text(
        color = brand_colors$navy,
        family = "Verdana",
        size = 12
      ),
      axis.text.y = element_text(
        color = brand_colors$navy,
        family = "Verdana",
        size = 12
      ),
      axis.line = element_line(color = "black", linewidth = 0.25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(
        color = brand_colors$navy,
        family = "Verdana",
        size = 10
      ),
      legend.key.size = unit(0.1, "cm"),
      plot.margin = margin(10, 60, 10, 10)
    )
}

# ---- Apply theme globally ----
apply_report_theme <- function() {
  theme_set(theme_report())
}

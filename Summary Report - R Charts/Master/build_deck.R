# ---- Libraries ----
library(officer)
library(rvg)
library(readr)  # needed so read_csv() is available when sourcing the chart files

# ---- Settings ----
template_path <- "NISS_ppt_template.pptx"   # adjust path if needed
output_path   <- "Summary_Report.pptx"

# Manual placement (inches)
left <- 1.0; top <- 1.4; width <- 9.0; height <- 4.8

# ---- Load template (fallback to blank if not found) ----
ppt <- try(read_pptx(template_path), silent = TRUE)
if (inherits(ppt, "try-error")) ppt <- read_pptx()

# ---- Collect scripts in numeric order (Chart xx.R in project root) ----
files <- list.files(
  path = ".",
  pattern = "^Chart\\s*\\d+.*\\.R$",
  full.names = TRUE
)
ord <- order(as.numeric(sub("^Chart\\s*(\\d+).*", "\\1", basename(files))))
files <- files[ord]

# ---- Build slides ----
for (f in files) {
  env <- new.env(parent = .GlobalEnv)  # ensures read_csv etc. are visible
  sys.source(f, envir = env)

  if (!exists("build_slide", envir = env, inherits = FALSE)) next
  slide <- env$build_slide()

  ppt <- add_slide(ppt, layout = slide$layout, master = slide$master)

# remove any ggplot titles/subtitles so only slide titles remain
slide$plot <- slide$plot +
  labs(title = NULL, subtitle = NULL) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())
# chart
ppt <- ph_with(
  ppt,
  dml(ggobj = slide$plot),
  location = ph_location(left = 0.30, top = 1.45, width = 12.7, height = 6)
)

# ---- Style controls (edit here to change all slides) ----
TITLE_FONT_SIZE    <- 24
SUBTITLE_FONT_SIZE <- 16
TITLE_COLOR        <- "#2B3555"
SUBTITLE_COLOR     <- "#2B3555"
TITLE_BOLD         <- TRUE
SUBTITLE_BOLD      <- FALSE
TITLE_FAMILY       <- "Verdana"
SUBTITLE_FAMILY    <- "Verdana"

# title + subtitle (top-left; strip line breaks; uses global styles)
title_text    <- gsub("\\n", " ", slide$title)
subtitle_text <- gsub("\\n", " ", slide$subtitle)

title_block <- block_list(
  fpar(
    ftext(
      title_text,
      fp_text(font.size = TITLE_FONT_SIZE, bold = TITLE_BOLD,
              color = TITLE_COLOR, font.family = TITLE_FAMILY)
    ),
    fp_p = fp_par(text.align = "center")
  )
)

subtitle_block <- block_list(
  fpar(
    ftext(
      subtitle_text,
      fp_text(font.size = SUBTITLE_FONT_SIZE, bold = SUBTITLE_BOLD,
              color = SUBTITLE_COLOR, font.family = SUBTITLE_FAMILY)
    ),
    fp_p = fp_par(text.align = "center")
  )
)

# Centered title directly above the chart
ppt <- ph_with(
  ppt, title_block,
  location = ph_location(
    left = 0.67,     # match chart left
    top    = 0.75,     # move down nearer the chart
    width  = 12,     # match chart width
    height = 0.45      # typical title height
  )
)

# Centered subtitle directly above the chart
ppt <- ph_with(
  ppt, subtitle_block,
  location = ph_location(
    left = 0.67,     # match chart left
    top    = 1.25,     # sits between title and chart
    width  = 12,     # match chart width
    height = 0.30      # typical subtitle height
  )
)
}

# ---- Export ----
print(ppt, target = output_path)
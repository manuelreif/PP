# Shared helpers for the blog scripts in blog/: arguments, loading the test
# references, table and plot style. Run all blog scripts from the package root.

suppressPackageStartupMessages(library(data.table))

blog_out <- "blog/output"

# Parses --key=value arguments
blog_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  get <- function(key, default = NULL) {
    hit <- grep(paste0("^--", key, "="), args, value = TRUE)
    if (length(hit)) sub(paste0("^--", key, "="), "", hit[1]) else default
  }
  list(label = get("label", "block1"))
}

# The references, test sets and pp_fit() from tests/testthat/helper-*.R, run
# against the installed PP
lade_test_helfer <- function() {
  suppressPackageStartupMessages(library(PP))
  for (f in sort(list.files("tests/testthat", "^helper-.*\\.R$", full.names = TRUE))) {
    sys.source(f, envir = globalenv())
  }
}

# Short description of the installed code version for the source notes
code_stand <- function() {
  sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "?")
  sprintf("PP %s, Commit `%s`", utils::packageDescription("PP")$Version, sha)
}

# --- status categories (used by the status matrix and its table) -----------

status_stufen <- c("ok", "teilweise", "falsch", "abbruch", "fehlt")
# U+FE0E asks for the text form of the warning sign, not the emoji
status_text <- c(ok = "✓", teilweise = "⚠\uFE0E", falsch = "✗", abbruch = "Abbruch", fehlt = "–")
status_farbe <- list(
  ok        = c(bg = "#DCFCE7", fg = "#166534"),
  teilweise = c(bg = "#FEF3C7", fg = "#92400E"),
  falsch    = c(bg = "#FEE2E2", fg = "#991B1B"),
  abbruch   = c(bg = "#FCA5A5", fg = "#7F1D1D"),
  fehlt     = c(bg = "#F6F8FA", fg = "#57606A")
)

# --- gt style (as in bench/table_gt.R) -------------------------------------

blog_gt_stil <- function(tab, id) {
  tab |>
    opt_table_font(font = list(google_font("Source Sans 3"), default_fonts())) |>
    opt_align_table_header("left") |>
    tab_style(cell_text(weight = "bold"), locations = list(cells_row_groups(), cells_title("title"))) |>
    tab_style(cell_text(color = "#57606A", size = px(12), transform = "uppercase"),
              locations = cells_column_spanners()) |>
    tab_options(
      table.font.size = px(14),
      table.border.top.style = "none",
      table.border.bottom.style = "none",
      heading.title.font.size = px(22),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = px(14),
      heading.border.bottom.style = "none",
      heading.padding = px(4),
      column_labels.font.weight = "bold",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.color = "#1F2328",
      column_labels.border.bottom.width = px(2),
      row_group.background.color = "#F6F8FA",
      row_group.border.top.color = "#D0D7DE",
      row_group.border.bottom.color = "#D0D7DE",
      table_body.hlines.color = "#EAEEF2",
      table_body.border.bottom.color = "#1F2328",
      stub.border.style = "none",
      data_row.padding = px(7),
      footnotes.font.size = px(12),
      source_notes.font.size = px(12),
      footnotes.marks = "numbers"
    ) |>
    opt_css(css = sprintf("
      #%1$s .gt_table { font-variant-numeric: tabular-nums; }
      #%1$s .gt_heading { padding-bottom: 10px; }
      #%1$s code { font-size: 0.95em; }
    ", id))
}

# --- ggplot style --------------------------------------------------------

akzent <- "#C2410C"
grau <- "#57606A"

theme_blog <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "Open Sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.25)),
      plot.subtitle = ggplot2::element_text(colour = grau),
      plot.caption = ggplot2::element_text(colour = grau, size = ggplot2::rel(0.8), hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "top",
      legend.justification = "left"
    )
}

# Saves a ggplot as PNG and SVG under blog/output/ (or the folder dir)
speichere_plot <- function(p, name, width = 8, height = 5, dir = blog_out) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  png_file <- file.path(dir, paste0(name, ".png"))
  ggplot2::ggsave(png_file, p, width = width, height = height, dpi = 200, bg = "white")
  ggplot2::ggsave(file.path(dir, paste0(name, ".svg")), p, width = width, height = height,
                  device = svglite::svglite, bg = "white")
  message("gespeichert: ", png_file, " (+ .svg)")
}

# German number format for labels, with a typographic minus sign
zahl_de <- function(x, digits = 2) {
  sub("^-", "\u2212", formatC(x, format = "f", digits = digits, decimal.mark = ","))
}

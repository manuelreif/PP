# Blog table of the benchmark runs: mean and spread per function, built with gt.
#
#   Rscript bench/table_gt.R [--label=baseline] [--out=bench/output/laufzeiten_<label>.html]
#                            [--db=bench/bench.duckdb]
#
# Data preparation uses data.table only; gt does the rendering.

suppressPackageStartupMessages(library(gt))
source("bench/read_bench.R")

args <- bench_args()
label <- if (is.null(args$label)) "baseline" else args$label
out <- if (is.null(args$out)) sprintf("bench/output/laufzeiten_%s.html", label) else args$out

dt <- bench_read(args$db, label)
s <- bench_summary(dt)
m <- dt[1]

# --- rows, groups, order -------------------------------------------------

fmt_int <- function(x) formatC(x, big.mark = ".", decimal.mark = ",", format = "d")

# one group per function and model; the templates take model, persons, items
gruppen <- data.table(
  funktion = c("PP_4pl", "PP_4pl", "PP_4pl", "PP_gpcm", "JKpp", "PV", "sim_gpcm"),
  modell = c("1PL", "2PL", "3PL", "GPCM", "3PL", "3PL", "GPCM"),
  vorlage = c(rep("`PP_4pl()` · %s, %s Personen × %s Items", 3),
              "`PP_gpcm()` · %s, %s Personen × %s Items, 5 Kategorien",
              "`JKpp()` · %s, %s Personen × %s Items, WLE-Objekt",
              "`PV()` · %s, %s Personen × %s Items, EAP-Objekt",
              "`sim_gpcm()` · %s, %s Personen × %s Items, 5 Kategorien")
)
gruppen[, g_ord := .I]
zeilen <- data.table(
  szenario = c("jk_wle", "pv", "sim_gpcm"),
  zeile = c("Jackknife (40 Teilstichproben)", "10 Plausible Values (MH)", "Simulation")
)

tbl <- merge(s, gruppen, by = c("funktion", "modell"))
tbl <- merge(tbl, zeilen, by = "szenario", all.x = TRUE)
tbl[is.na(zeile), zeile := methode]
tbl[, gruppe := sprintf(vorlage, modell, fmt_int(n_personen), n_items)]
tbl[, m_ord := match(methode, c("MLE", "WLE", "MAP", "EAP"))]
setorder(tbl, g_ord, m_ord)

# Newton-Raphson iterations: one call ("40"), several calls ("40 × 500", or
# the mean per call if they differ), none (EAP, PV, simulation: NA -> "–")
tbl[, iter := fcase(
  is.na(nr_aufrufe) | nr_aufrufe == 0L, NA_character_,
  nr_aufrufe == 1L, fmt_int(iterationen),
  iterationen == nr_aufrufe * iter_max, sprintf("%s × %s", nr_aufrufe, fmt_int(iter_max)),
  default = sprintf("%s × Ø %s", nr_aufrufe, fmt_int(round(iterationen / nr_aufrufe)))
)]

# gt draws all boxplot nanoplots on one common scale, so each run enters as
# its percent deviation from the scenario median: comparable across rows.
rel <- dt[order(durchlauf), .(abw = paste(format(100 * (sekunden / median(sekunden) - 1), digits = 4,
                                                 scientific = FALSE, trim = TRUE), collapse = " ")),
          by = szenario]
tbl <- merge(tbl, rel, by = "szenario", sort = FALSE)
tbl <- tbl[, .(gruppe, zeile, szenario, iter, mittel, sd, median, iqr, min, max, vk, abw)]

# --- header and notes ----------------------------------------------------

cpu <- gsub("\\s+", " ", gsub("\\((R|TM)\\)|\\d+th Gen ", "", m$cpu))
r_ver <- sub("^R version (\\S+).*$", "R \\1", m$r_version)
n_runs <- s[, if (min(n) == max(n)) as.character(n[1]) else sprintf("%d–%d", min(n), max(n))]
titel <- if (label == "baseline") "Laufzeiten von PP 0.6.4-1 vor dem Umbau" else
  sprintf("Laufzeiten von PP %s (%s)", m$pp_version, label)
untertitel <- sprintf("%s Durchläufe je Szenario · %s · %s · ein Thread", n_runs, trimws(cpu), r_ver)
quelle <- sprintf(
  "**Messung:** `bench/bench.R`, PP %s, Commit `%s`%s, %s · **Daten:** in R simuliert (`bench/scenarios.R`) · Verstrichene Zeit nach einem Aufwärmlauf, die Durchläufe verschränkt in zufälliger Reihenfolge.",
  m$pp_version, m$git_sha, if (isTRUE(m$git_dirty)) " (mit lokalen Änderungen)" else "",
  format(m$messung_zeitpunkt, "%d.%m.%Y", tz = Sys.timezone())
)

akzent <- "#C2410C"

# --- table ---------------------------------------------------------------

tab <- gt(tbl, rowname_col = "zeile", groupname_col = "gruppe", process_md = TRUE,
          locale = "de", id = "ppbench") |>
  cols_hide(szenario) |>
  tab_header(title = titel, subtitle = untertitel) |>
  cols_nanoplot(
    columns = abw, plot_type = "boxplot", new_col_name = "verteilung",
    new_col_label = "Verteilung", after = "vk",
    options = nanoplot_options(
      data_bar_stroke_color = akzent, data_bar_fill_color = akzent,
      y_val_fmt_fn = function(x) sprintf("%+.1f %%", x)
    )
  ) |>
  cols_merge_range(col_begin = min, col_end = max, sep = "–") |>
  fmt_number(columns = c(mittel, sd, median, iqr, min, max), n_sigfig = 3) |>
  fmt_percent(columns = vk, decimals = 1) |>
  sub_missing(columns = iter, missing_text = "–") |>
  cols_label(
    iter = "Iterationen", mittel = "Mittelwert", sd = "SD", median = "Median", iqr = "IQR",
    min = "Min–Max", vk = "VK"
  ) |>
  tab_spanner("Laufzeit in Sekunden", columns = c(mittel, sd, median, iqr, min)) |>
  tab_spanner("Streuung", columns = c(vk, verteilung)) |>
  cols_align("right", columns = c(iter, mittel, sd, median, iqr, min, vk)) |>
  cols_align("center", columns = verteilung) |>
  tab_footnote(
    md("Newton-Raphson-Durchläufe. Der alte Code rechnet für alle Personen weiter, bis die letzte konvergiert ist oder `maxsteps` erreicht ist (40 bei `PP_4pl()`, 500 bei `JKpp()`). EAP, Plausible Values und Simulation iterieren nicht. Gezählt sind die ausgeführten Durchläufe; `PP_gpcm()` meldet in `nsteps` einen weniger."),
    locations = cells_column_labels(iter)
  ) |>
  tab_footnote(
    "Variationskoeffizient: Standardabweichung geteilt durch den Mittelwert.",
    locations = cells_column_labels(vk)
  ) |>
  tab_footnote(
    "Boxplot der Einzelläufe als Abweichung vom Median der Zeile in Prozent, alle Zeilen auf derselben Skala.",
    locations = cells_column_labels(verteilung)
  ) |>
  tab_footnote(
    md("Eine einzige Person pendelt im Newton-Verfahren. Weil der alte Code erst abbricht, wenn alle Personen konvergiert sind, rechnen alle 40 Teilstichproben bis `maxsteps = 500`."),
    locations = cells_stub(rows = szenario == "jk_wle")
  ) |>
  tab_source_note(md(quelle)) |>
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
  opt_css(css = "
    #ppbench .gt_table { font-variant-numeric: tabular-nums; }
    #ppbench .gt_heading { padding-bottom: 10px; }
    #ppbench code { font-size: 0.95em; }
  ")

dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
gtsave(tab, out)
message("gespeichert: ", out)

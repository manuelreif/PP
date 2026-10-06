# Test balance table and the burn-down of open problems.
#
#   Rscript blog/testbilanz.R [--label=block1]
#
# Reads blog/output/testbilanz_<label>.rds (blog/testbilanz_daten.R) and
# writes testbilanz_<label>.html and offene_fehler_<label>.png/.svg. The
# planned burn-down comes from the block numbers in the skip messages; every
# other testbilanz_*.rds in blog/output adds a measured point.

suppressPackageStartupMessages({
  library(gt)
  library(ggplot2)
})
source("blog/gemeinsam.R")
args <- blog_args()
tests <- readRDS(file.path(blog_out, sprintf("testbilanz_%s.rds", args$label)))

# --- table ------------------------------------------------------------------

ids_sortiert <- function(x) {
  x <- unique(na.omit(x))
  x[order(substr(x, 1, 1), as.integer(substring(x, 2)))]
}
bl <- tests[, .(
  tests = .N,
  bestanden = sum(bestanden),
  skips = sum(!is.na(skip)),
  ids = paste(ids_sortiert(id), collapse = ", "),
  bloecke = paste(sort(unique(na.omit(block))), collapse = ", ")
), keyby = .(ord, bereich)]
bl[, gruppe := fifelse(ord == 10, "Vorher", "Neu in Block 1")]
n_ids <- length(ids_sortiert(tests[ord < 10, id]))
summe <- bl[gruppe != "Vorher", .(ord = 9.5, bereich = "Summe neu", tests = sum(tests),
                                  bestanden = sum(bestanden), skips = sum(skips),
                                  ids = sprintf("%d IDs", n_ids), bloecke = "",
                                  gruppe = "Neu in Block 1")]
bl <- rbind(bl, summe)[order(ord)]
bl[ids == "", ids := NA_character_]
bl[bloecke == "", bloecke := NA_character_]

titel <- "Das Sicherheitsnetz in Zahlen"
untertitel <- sprintf("Testlauf gegen PP 0.6.4-1 · %s Tests, davon %s bekannte Fehler als übersprungene Tests",
                      nrow(tests), sum(!is.na(tests$skip)))
quelle <- sprintf(
  "**Lauf:** `blog/testbilanz_daten.R`, %s, %s · Übersprungene Tests beschreiben das korrekte Verhalten und laufen mit `PP_KNOWN_BUGS=run`; auf dem alten Code schlagen sie alle fehl.",
  tests$code[1], format(tests$zeitpunkt[1], "%d.%m.%Y"))

tab <- gt(bl[, .(gruppe, bereich, tests, bestanden, skips, ids, bloecke)],
          rowname_col = "bereich", groupname_col = "gruppe", locale = "de", id = "pptests") |>
  tab_header(title = titel, subtitle = untertitel) |>
  cols_label(tests = "Tests", bestanden = "Prüfungen bestanden", skips = "Bekannte Fehler",
             ids = "Problem-IDs", bloecke = "Behoben in Block") |>
  fmt_integer(columns = c(tests, bestanden, skips)) |>
  sub_missing(columns = c(ids, bloecke), missing_text = "–") |>
  cols_align("right", columns = c(tests, bestanden, skips)) |>
  tab_style(cell_text(weight = "bold"), locations = cells_body(rows = bereich == "Summe neu")) |>
  tab_style(cell_text(weight = "bold"), locations = cells_stub(rows = bereich == "Summe neu")) |>
  tab_footnote("Einzelne Prüfungen (expect_*) in bestandenen Tests.",
               locations = cells_column_labels(bestanden)) |>
  tab_footnote("Tests, die einen Fehler aus der Problemliste beschreiben und bis zu dessen Behebung übersprungen werden.",
               locations = cells_column_labels(skips)) |>
  tab_footnote("Die Referenzimplementierungen werden gegen geschlossene Formeln, optimize() und integrate() geprüft.",
               locations = cells_stub(rows = bereich == "Referenzen (Selbsttest)")) |>
  tab_source_note(md(quelle)) |>
  blog_gt_stil("pptests")
out <- file.path(blog_out, sprintf("testbilanz_%s.html", args$label))
gtsave(tab, out)
message("gespeichert: ", out)

# --- burn-down ----------------------------------------------------------------

offen <- tests[!is.na(block)]
geplant <- data.table(nach = 1:10)
geplant[, offen := vapply(nach, function(k) sum(offen$block > k), 0L)]

# measured points: number of skips in every stored testbilanz_<label>.rds
dateien <- list.files(blog_out, "^testbilanz_block[0-9]+\\.rds$", full.names = TRUE)
gemessen <- rbindlist(lapply(dateien, function(f) {
  d <- readRDS(f)
  data.table(nach = as.integer(sub("^block", "", d$label[1])), offen = sum(!is.na(d$block)))
}))

p <- ggplot(geplant, aes(nach, offen)) +
  geom_step(colour = grau, linewidth = 0.6, linetype = "22", direction = "hv") +
  geom_point(colour = grau, size = 1.8) +
  geom_text(aes(label = offen), colour = grau, hjust = 0, vjust = -0.5, nudge_x = 0.1,
            size = 3.4, family = "Open Sans") +
  geom_point(data = gemessen, colour = akzent, size = 3.2) +
  scale_x_continuous(breaks = 1:10, labels = paste("Block", 1:10)) +
  scale_y_continuous(limits = c(0, max(geplant$offen) * 1.12), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Offene Fehler im Lauf der Serie",
    subtitle = "Übersprungene Tests nach jedem Block · grau: geplant laut Skip-Vermerk, orange: gemessen",
    x = NULL, y = "Übersprungene Tests",
    caption = sprintf("Jeder übersprungene Test nennt den Block, der den Fehler behebt (z. B. „A7 - fixed in block 4“).\nStand: %s.",
                      gsub("`", "", tests$code[1]))
  ) +
  theme_blog() +
  theme(panel.grid.major.x = element_blank())
speichere_plot(p, sprintf("offene_fehler_%s", args$label), width = 8, height = 4.2)

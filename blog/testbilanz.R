# Test balance table and the burn-down of open problems.
#
#   Rscript blog/testbilanz.R [--label=block1]
#
# Reads blog/output/testbilanz_<label>.rds (blog/testbilanz_daten.R) and
# writes testbilanz_<label>.html and offene_fehler_<label>.png/.svg. Every
# testbilanz_<label>.rds in blog/output adds a point to the burn-down. Problem
# IDs and block numbers stay internal and are not shown.

suppressPackageStartupMessages({
  library(gt)
  library(ggplot2)
})
source("blog/gemeinsam.R")
args <- blog_args()
tests <- readRDS(file.path(blog_out, sprintf("testbilanz_%s.rds", args$label)))

# --- table ------------------------------------------------------------------

bl <- tests[, .(
  tests = .N,
  bestanden = sum(bestanden),
  skips = sum(!is.na(skip))
), keyby = .(ord, bereich)]
bl[, gruppe := fifelse(ord == 10, "Vorher", "Neu")]
summe <- bl[gruppe != "Vorher", .(ord = 9.5, bereich = "Summe neu", tests = sum(tests),
                                  bestanden = sum(bestanden), skips = sum(skips), gruppe = "Neu")]
bl <- rbind(bl, summe)[order(ord)]

titel <- "Das Sicherheitsnetz in Zahlen"
untertitel <- sprintf("Testlauf gegen PP 0.6.4-1 · %s Tests, davon %s bekannte Fehler als übersprungene Tests",
                      nrow(tests), sum(!is.na(tests$skip)))
quelle <- sprintf(
  "**Lauf:** `blog/testbilanz_daten.R`, %s, %s · Übersprungene Tests beschreiben das korrekte Verhalten und laufen mit `PP_KNOWN_BUGS=run`; auf dem alten Code schlagen sie alle fehl.",
  tests$code[1], format(tests$zeitpunkt[1], "%d.%m.%Y"))

tab <- gt(bl[, .(gruppe, bereich, tests, bestanden, skips)],
          rowname_col = "bereich", groupname_col = "gruppe", locale = "de", id = "pptests") |>
  tab_header(title = titel, subtitle = untertitel) |>
  cols_label(tests = "Tests", bestanden = "Prüfungen bestanden", skips = "Bekannte Fehler") |>
  fmt_integer(columns = c(tests, bestanden, skips)) |>
  cols_align("right", columns = c(tests, bestanden, skips)) |>
  tab_style(cell_text(weight = "bold"), locations = cells_body(rows = bereich == "Summe neu")) |>
  tab_style(cell_text(weight = "bold"), locations = cells_stub(rows = bereich == "Summe neu")) |>
  tab_footnote("Einzelne Prüfungen (expect_*) in bestandenen Tests.",
               locations = cells_column_labels(bestanden)) |>
  tab_footnote("Tests, die einen bekannten Fehler beschreiben und bis zu dessen Behebung übersprungen werden.",
               locations = cells_column_labels(skips)) |>
  tab_footnote("Die Referenzimplementierungen werden gegen geschlossene Formeln, optimize() und integrate() geprüft.",
               locations = cells_stub(rows = bereich == "Referenzen (Selbsttest)")) |>
  tab_source_note(md(quelle)) |>
  blog_gt_stil("pptests")
out <- file.path(blog_out, sprintf("testbilanz_%s.html", args$label))
gtsave(tab, out)
message("gespeichert: ", out)

# --- burn-down ----------------------------------------------------------------

# measured points: number of skips in every stored testbilanz_<label>.rds
dateien <- list.files(blog_out, "^testbilanz_block[0-9]+\\.rds$", full.names = TRUE)
gemessen <- rbindlist(lapply(dateien, function(f) {
  d <- readRDS(f)
  data.table(teil = as.integer(sub("^block", "", d$label[1])), offen = sum(!is.na(d$skip)))
}))
setorder(gemessen, teil)

p <- ggplot(gemessen, aes(teil, offen)) +
  (if (nrow(gemessen) > 1) geom_line(colour = akzent, linewidth = 0.6)) +
  geom_point(colour = akzent, size = 3.2) +
  geom_text(aes(label = offen), colour = akzent, hjust = 0, vjust = -0.6, nudge_x = 0.08,
            size = 3.6, family = "Open Sans") +
  scale_x_continuous(breaks = 0:10, labels = paste("Teil", 0:10), limits = c(min(gemessen$teil) - 0.3, 10.3)) +
  scale_y_continuous(limits = c(0, max(gemessen$offen) * 1.15), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "Offene Fehler im Lauf der Serie",
    subtitle = "Übersprungene Tests nach jedem Teil",
    x = NULL, y = "Übersprungene Tests",
    caption = sprintf("Jeder bekannte Fehler hat übersprungene Tests. Ist er behoben, laufen sie wieder und müssen bestehen.\nStand: %s.",
                      gsub("`", "", tests$code[1]))
  ) +
  theme_blog() +
  theme(panel.grid.major.x = element_blank())
speichere_plot(p, sprintf("offene_fehler_%s", args$label), width = 8, height = 4.2)

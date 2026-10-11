# Table "PP und eRm": person parameters of PP for every eRm model, compared
# with eRm::person.parameter() (maximum likelihood).
#
#   Rscript blog/eRm/erm.R [--label=block8]
#
# Reads blog/eRm/output/erm_<label>.rds (blog/eRm/erm_daten.R) and writes
# blog/eRm/output/erm_<label>.html.

suppressPackageStartupMessages(library(gt))
source("blog/gemeinsam.R")
args <- blog_args()
ordner <- "blog/eRm/output"
z <- readRDS(file.path(ordner, sprintf("erm_%s.rds", args$label)))

namen <- c(RM = "Rasch-Modell", LLTM = "Linear-logistisches Testmodell", RSM = "Rating-Scale-Modell",
           PCM = "Partial-Credit-Modell", LRSM = "Lineares Rating-Scale-Modell",
           LPCM = "Lineares Partial-Credit-Modell")
z[, zeile := sprintf("%s (`%s()`)", namen[modell], modell)]
z[, gruppe := fifelse(design %in% c("", "mit fehlenden Antworten"), "Ein Messzeitpunkt",
                      "Mehrere Messzeitpunkte (virtuelle Items)")]
z[, design := fifelse(design == "", "–", design)]
z[, items_text := fifelse(virtuell == items, as.character(items),
                          sprintf("%d (%d virtuell)", items, virtuell))]
gruppe_zeile <- z[, which(!erm_moeglich)]

titel <- "PP und eRm"
untertitel <- "Personenparameter für jedes eRm-Modell: Maximum-Likelihood-Schätzer von PP gegen eRm::person.parameter()"
quelle <- sprintf(
  "**Lauf:** `blog/eRm/erm_daten.R`, %s, eRm %s, %s · Simulierte Daten. Verglichen werden alle Personen mit endlichem Schätzer, die eRm nicht interpoliert.",
  z$code[1], z$erm_version[1], format(z$zeitpunkt[1], "%d.%m.%Y"))

tab <- gt(z[, .(gruppe, zeile, design, personen, items_text, verglichen, max_theta, max_se,
                wle_geschaetzt, auffaellig)],
          rowname_col = "zeile", groupname_col = "gruppe", locale = "de", id = "pperm") |>
  tab_header(title = titel, subtitle = untertitel) |>
  fmt_markdown(columns = zeile) |>
  tab_spanner("Daten", columns = c(design, personen, items_text)) |>
  tab_spanner("Vergleich mit eRm (ML)", columns = c(verglichen, max_theta, max_se)) |>
  tab_spanner("PP mit WLE", columns = c(wle_geschaetzt, auffaellig)) |>
  cols_label(design = "Design", personen = "Personen", items_text = "Items",
             verglichen = "Personen", max_theta = "max. |Δθ|", max_se = "max. |ΔSE|",
             wle_geschaetzt = "geschätzt", auffaellig = "auffällig") |>
  fmt_integer(columns = c(personen, verglichen, wle_geschaetzt, auffaellig)) |>
  fmt_scientific(columns = c(max_theta, max_se), decimals = 1) |>
  sub_missing(columns = c(verglichen, max_theta, max_se), missing_text = "–") |>
  cols_align("right", columns = c(personen, items_text, verglichen, max_theta, max_se,
                                  wle_geschaetzt, auffaellig)) |>
  tab_footnote("eRm schätzt bei Gruppenkontrasten keine Personenparameter. PP schätzt sie: Jede Person beantwortet die virtuellen Items ihrer Gruppe.",
               locations = cells_stub(rows = gruppe_zeile)) |>
  tab_footnote("Mit Messzeitpunkten oder Gruppen hat jede Kombination aus Item, Zeitpunkt und Gruppe eigene Parameter. Jede Person bekommt eine Fähigkeit über alle Zeitpunkte.",
               locations = cells_column_labels(items_text)) |>
  tab_footnote("Weighted Likelihood hat auch für Personen mit null oder voller Punktzahl einen endlichen Schätzer.",
               locations = cells_column_labels(wle_geschaetzt)) |>
  tab_footnote("Personen mit |lz*| > 1,96 (Personenfit nach Snijders).",
               locations = cells_column_labels(auffaellig)) |>
  tab_source_note(md(quelle)) |>
  blog_gt_stil("pperm")
out <- file.path(ordner, sprintf("erm_%s.html", args$label))
gtsave(tab, out)
message("gespeichert: ", out)

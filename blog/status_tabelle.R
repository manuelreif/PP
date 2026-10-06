# Blog tables of the status matrix "What works?" (gt, German).
#
#   Rscript blog/status_tabelle.R [--label=block1]
#
# Reads blog/output/status_<label>.rds (from blog/status_daten.R) and writes
# three tables: status_schaetzer_<label>.html, status_folge_<label>.html and
# status_sonder_<label>.html.

suppressPackageStartupMessages(library(gt))
source("blog/gemeinsam.R")
args <- blog_args()
st <- readRDS(file.path(blog_out, sprintf("status_%s.rds", args$label)))

# --- cell content ---------------------------------------------------------

# Symbol in the first line, a detail in the second: "1 von 20" for partly
# wrong cells, the largest deviation for wrong ones, "NaN" where all wrong
# values are missing
zelle_html <- function(status, n, n_schlecht, n_nan, abw) {
  nur_nan <- !is.na(n_nan) & n_nan > 0 & n_nan == n_schlecht
  detail <- fcase(
    status == "teilweise" & nur_nan, sprintf("%d von %d NaN", n_schlecht, n),
    status == "teilweise", sprintf("%d von %d", n_schlecht, n),
    status == "falsch" & nur_nan & n_schlecht < n, sprintf("%d von %d NaN", n_schlecht, n),
    status == "falsch" & nur_nan, "NaN",
    status == "falsch" & is.finite(abw), sprintf("Δ %s", zahl_de(abw)),
    status == "falsch", "NaN",
    default = ""
  )
  fg <- vapply(status, function(s) status_farbe[[s]][["fg"]], "")
  sprintf('<div style="display:inline-block;line-height:1.2;color:%s"><span style="font-weight:700;white-space:nowrap">%s</span>%s</div>',
          fg, status_text[status], ifelse(detail == "", "", sprintf('<br><span style="font-size:11px">%s</span>', detail)))
}
st[, html := zelle_html(status, n, n_schlecht, n_nan, abw)]

# Colours each body cell by the status in the matching hidden column
faerben <- function(tab, wide, spalten) {
  for (sp in spalten) {
    stat <- wide[[paste0(sp, "__status")]]
    for (s in unique(na.omit(stat))) {
      tab <- tab_style(tab, style = cell_fill(color = status_farbe[[s]][["bg"]]),
                       locations = cells_body(columns = all_of(sp), rows = which(stat == s)))
    }
  }
  tab
}

# One numbered footnote per known cause (column `grund` of the status data),
# at the column label, the row label or the cells themselves. `zellen` are the
# cells of one table with the column name `sp` and the row key `key`, which
# matches `wide$key`.
fussnoten <- function(tab, wide, zellen) {
  zellen <- zellen[!is.na(grund)]
  for (g in unique(zellen$grund)) {
    z <- zellen[grund == g]
    ort <- z$ort[1]
    orte <- switch(ort,
      spalte = list(cells_column_labels(columns = all_of(unique(z$sp)))),
      zeile = list(cells_stub(rows = match(unique(z$key), wide$key))),
      zelle = lapply(seq_len(nrow(z)), function(i) {
        cells_body(columns = all_of(z$sp[i]), rows = match(z$key[i], wide$key))
      })
    )
    tab <- tab_footnote(tab, g, locations = orte)
  }
  tab
}

legende <- paste(
  "**✓** alle Personen innerhalb der Toleranz ·", sprintf("**%s**", status_text[["teilweise"]]),
  "einzelne Personen falsch (höchstens 20 %) ·",
  "**✗** systematisch falsch · **Abbruch** mit Fehlermeldung · **–** nicht angeboten.",
  "Δ = größte Abweichung von der Referenz."
)
toleranz <- paste(
  "**Toleranz:** 0,01 auf der θ-Skala (Schätzer, Standardfehler, EAP), 0,02 beim Jackknife,",
  "1e-4 bei den Fit-Indizes, 4 Monte-Carlo-Standardfehler bei Plausible Values."
)
quelle <- sprintf(
  "**Messung:** `blog/status_daten.R`, %s, %s · **Referenzen:** reines R mit numerischen Ableitungen, `uniroot`, Posterior auf 4001 Gitterpunkten, Lehrbuch-Jackknife (`tests/testthat/helper-2-reference.R`) · **Daten:** in R simuliert, 20–25 Personen je Modell.",
  st$code[1], format(st$zeitpunkt[1], "%d.%m.%Y")
)
stand <- if (args$label == "block1") "Stand vor dem Umbau (PP 0.6.4-1)" else sprintf("Stand nach Teil %s", sub("^block", "", args$label))

speichere_tab <- function(tab, name) {
  out <- file.path(blog_out, sprintf("status_%s_%s.html", name, args$label))
  gtsave(tab, out)
  message("gespeichert: ", out)
}

# --- table 1: estimators --------------------------------------------------

t1 <- st[tabelle == "schaetzer"]
t1[, sp := paste(spalte, mass, sep = "_")]
methoden <- c("MLE", "WLE", "MAP", "EAP", "Robust")
spalten1 <- as.vector(t(outer(methoden, c("theta", "se"), paste, sep = "_")))
w1 <- dcast(t1, zeile ~ sp, value.var = "html")
s1 <- dcast(t1, zeile ~ sp, value.var = "status")
setnames(s1, spalten1, paste0(spalten1, "__status"))
w1 <- merge(w1, s1, by = "zeile")[match(c("1PL", "2PL", "3PL", "4PL", "PCM", "GPCM", "gemischt"), zeile)]
w1[, key := zeile]
setcolorder(w1, c("zeile", spalten1))

tab1 <- gt(w1, rowname_col = "zeile", locale = "de", id = "ppstatus1") |>
  cols_hide(c(ends_with("__status"), key)) |>
  fmt_markdown(columns = all_of(spalten1)) |>
  tab_header(title = "Was funktioniert? Personenschätzer",
             subtitle = paste(stand, "· Schätzer θ̂ und Standardfehler je Modell und Methode")) |>
  cols_label(.list = setNames(as.list(rep(c("θ̂", "SE"), length(methoden))), spalten1)) |>
  cols_align("center", columns = all_of(spalten1)) |>
  cols_width(all_of(spalten1) ~ px(82)) |>
  tab_source_note(md(legende)) |>
  tab_source_note(md(toleranz)) |>
  tab_source_note(md(quelle))
for (me in methoden) {
  tab1 <- tab_spanner(tab1, label = me, id = me, columns = all_of(paste0(me, c("_theta", "_se"))))
}
tab1 <- tab1 |>
  tab_footnote(md("Robust konvergiert im alten Code nur linear: Mit dem Default `exac = 0.001` liegt θ̂ bis 0,003 neben der Lösung, innerhalb der Toleranz von 0,01."),
               locations = cells_column_spanners("Robust"))
tab1 <- fussnoten(tab1, w1, t1[, .(sp, key = zeile, ort, grund)])
tab1 <- faerben(tab1, w1, spalten1) |> blog_gt_stil("ppstatus1")
speichere_tab(tab1, "schaetzer")

# --- table 2: downstream functions -----------------------------------------

t2 <- st[tabelle == "folge"]
spalten2 <- c("2PL", "PCM", "GPCM", "gemischt")
w2 <- dcast(t2, zeile ~ spalte, value.var = "html")
s2 <- dcast(t2, zeile ~ spalte, value.var = "status")
setnames(s2, spalten2, paste0(spalten2, "__status"))
zeilen2 <- unique(t2$zeile)
w2 <- merge(w2, s2, by = "zeile")[match(zeilen2, zeile)]
w2[, key := zeile]
w2[, gruppe := sub(":.*$", "", zeile)]
w2[, zeile := sub("^[^:]*: ?", "", zeile)]
w2[gruppe == "Simulation", zeile := "Häufigkeiten wie im Modell"]
w2[, gruppe := fcase(gruppe == "JKpp", "Jackknife · JKpp()",
                     gruppe == "PV", "Plausible Values · PV()",
                     gruppe == "Pfit", "Personenfit · Pfit() an der WLE",
                     default = "Simulation · sim_4pl(), sim_gpcm()")]
setcolorder(w2, c("gruppe", "zeile", spalten2))

tab2 <- gt(w2, rowname_col = "zeile", groupname_col = "gruppe", locale = "de", id = "ppstatus2") |>
  cols_hide(c(ends_with("__status"), key)) |>
  fmt_markdown(columns = all_of(spalten2)) |>
  tab_header(title = "Was funktioniert? Jackknife, Plausible Values, Personenfit",
             subtitle = paste(stand, "· je Funktion und Modell")) |>
  cols_align("center", columns = all_of(spalten2)) |>
  cols_width(all_of(spalten2) ~ px(110)) |>
  tab_footnote("Eine Antwortfolge 2000-mal repliziert, 20 Draws je Person; verglichen werden Mittelwert und SD der Draws mit dem exakten Posterior.",
               locations = cells_stub(rows = zeile == "Ziehung aus dem Posterior")) |>
  tab_source_note(md(legende)) |>
  tab_source_note(md(toleranz)) |>
  tab_source_note(md(quelle))
tab2 <- fussnoten(tab2, w2, t2[, .(sp = spalte, key = zeile, ort, grund)])
tab2 <- faerben(tab2, w2, spalten2) |> blog_gt_stil("ppstatus2")
speichere_tab(tab2, "folge")

# --- table 3: special cases -------------------------------------------------

t3 <- st[tabelle == "sonder"]
spalten3 <- c("Schätzer", "EAP", "JKpp", "PV", "Pfit")
w3 <- dcast(t3, zeile ~ spalte, value.var = "html")
s3 <- dcast(t3, zeile ~ spalte, value.var = "status")
for (sp in setdiff(spalten3, names(w3))) w3[, (sp) := NA_character_]
for (sp in setdiff(spalten3, names(s3))) s3[, (sp) := NA_character_]
setnames(s3, spalten3, paste0(spalten3, "__status"))
w3 <- merge(w3, s3, by = "zeile")[match(unique(t3$zeile), zeile)]
w3[, key := zeile]
setcolorder(w3, c("zeile", spalten3))

tab3 <- gt(w3, rowname_col = "zeile", locale = "de", id = "ppstatus3") |>
  cols_hide(c(ends_with("__status"), key)) |>
  fmt_markdown(columns = all_of(spalten3)) |>
  sub_missing(columns = all_of(spalten3), missing_text = "") |>
  tab_header(title = "Was funktioniert? Sonderfälle",
             subtitle = paste(stand, "· 2PL-Testsatz, sofern nicht anders angegeben")) |>
  cols_label(Schätzer = "Schätzer", EAP = "EAP", JKpp = "Jackknife", PV = "Plausible Values",
             Pfit = "Personenfit") |>
  cols_align("center", columns = all_of(spalten3)) |>
  cols_width(all_of(spalten3) ~ px(110)) |>
  tab_footnote(md("Schätzer: WLE bei fehlenden Antworten und Kategorien ≥ 10, MLE bei der Person ohne Antworten und am Rateniveau (Ergebnis bei `maxsteps` 40 und 41 verglichen), sonst MAP. Personenfit: lz\\* bei fehlenden Antworten, Jackknife-SE von lz bei personenspezifischen Priors."),
               locations = cells_column_labels(c(Schätzer, Pfit))) |>
  tab_source_note(md(paste(legende, "Leere Zellen: nicht geprüft."))) |>
  tab_source_note(md(toleranz)) |>
  tab_source_note(md(quelle))
tab3 <- fussnoten(tab3, w3, t3[, .(sp = spalte, key = zeile, ort, grund)])
tab3 <- faerben(tab3, w3, spalten3) |> blog_gt_stil("ppstatus3")
speichere_tab(tab3, "sonder")

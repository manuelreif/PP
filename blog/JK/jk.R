# Pictures of the jackknife:
#   jk_prinzip              one person: leave-one-out estimates, pseudovalues
#                           and the SE of both versions
#   jk_faktor               JK-SE relative to the WLE-SE over the test length
#   jk_fehlende_antworten   a person with 6 of 12 items answered
#   jk_extremer_ausfall     an MLE leave-one-out sample without a finite estimate
#
#   Rscript blog/JK/jk.R [--label=block5]
#
# Reads and writes in blog/JK/output/ (data: blog/JK/jk_daten.R).

suppressPackageStartupMessages(library(ggplot2))
source("blog/gemeinsam.R")
args <- blog_args()
ordner <- "blog/JK/output"
d <- readRDS(file.path(ordner, sprintf("jk_%s.rds", args$label)))
dunkel <- "#1F2328"
blau <- "#0969DA"
hell <- "#D0D7DE"
stand <- sprintf("Stand: %s.", gsub("`", "", d$code))
farben <- c("PP 0.6.4-1" = akzent, "Lehrbuch" = blau)
schrift <- "Open Sans"

# Text in the plot area; box = TRUE puts it on white, above lines behind it
notiz <- function(x, y, label, hjust = 0, colour = grau, size = 3.1, box = FALSE, ...) {
  if (box) {
    annotate("label", x = x, y = y, label = label, hjust = hjust, colour = colour, size = size,
             family = schrift, lineheight = 0.95, fill = "white", linewidth = 0,
             label.padding = unit(0.2, "lines"), ...)
  } else {
    annotate("text", x = x, y = y, label = label, hjust = hjust, colour = colour, size = size,
             family = schrift, lineheight = 0.95, ...)
  }
}

# An estimate +- SE as a bar with end ticks, the estimate as a point and the
# SE written next to the bar
intervall <- function(y, mitte, se, farbe, text_se = TRUE) {
  list(
    annotate("segment", x = mitte - se, xend = mitte + se, y = y, yend = y, colour = farbe,
             linewidth = 1.1, lineend = "butt"),
    annotate("segment", x = c(mitte - se, mitte + se), xend = c(mitte - se, mitte + se),
             y = y - 0.09, yend = y + 0.09, colour = farbe, linewidth = 0.8),
    annotate("point", x = mitte, y = y, colour = farbe, size = 3),
    if (text_se) notiz(mitte + se, y, sprintf("  ± %s", zahl_de(se)), colour = farbe, size = 3.3,
                       fontface = "bold")
  )
}

antwort <- function(x) factor(fifelse(is.na(x), "nicht bearbeitet", fifelse(x == 1, "gelöst", "nicht gelöst")),
                              levels = c("gelöst", "nicht gelöst", "nicht bearbeitet"))
punkte_skala <- list(
  scale_fill_manual(values = c("gelöst" = dunkel, "nicht gelöst" = "white", "nicht bearbeitet" = hell),
                    name = "Item"),
  scale_colour_manual(values = c("gelöst" = dunkel, "nicht gelöst" = dunkel, "nicht bearbeitet" = grau),
                      name = "Item")
)
zeilen_theme <- theme_blog() +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(hjust = 0, colour = dunkel,
                                                                          lineheight = 0.95))

# --- jk_prinzip -------------------------------------------------------------------

p <- d$prinzip
it <- copy(p$items)[, antwort := antwort(x)]
L <- p$L
y_loo <- 4; y_ps <- 3; y_ref <- 1.9; y_alt <- 1.35; y_mod <- 0.8
x_rechts <- max(it$ps_ref) + 0.45
zeilen <- c(
  sprintf("Leave-one-out-Schätzer\nθ̂ ohne Item j, je %d Items", L - 1),
  sprintf("Pseudowerte\n%d · θ̂ − %d · (θ̂ ohne Item j)", L, L - 1),
  "Lehrbuch: Mittel ± SE\nder Pseudowerte",
  sprintf("PP 0.6.4-1: SE aus den\nLeave-one-out-Schätzern"),
  "WLE ± SE aus dem Modell"
)
streu_loo <- diff(range(it$loo))
streu_ps <- diff(range(it$ps_ref))

p_pr <- ggplot(it) +
  geom_vline(xintercept = p$theta, colour = grau, linetype = "22", linewidth = 0.4) +
  geom_segment(aes(x = loo, xend = ps_ref, y = y_loo, yend = y_ps), colour = hell, linewidth = 0.5) +
  annotate("segment", x = min(it$loo), xend = max(it$loo), y = y_loo + 0.32, yend = y_loo + 0.32,
           colour = dunkel, linewidth = 0.4) +
  annotate("segment", x = min(it$ps_ref), xend = max(it$ps_ref), y = y_ps - 0.32, yend = y_ps - 0.32,
           colour = dunkel, linewidth = 0.4) +
  notiz(mean(range(it$loo)), y_loo + 0.47, sprintf("Spannweite %s", zahl_de(streu_loo)),
        hjust = 0.5, colour = dunkel) +
  notiz(mean(range(it$ps_ref)), y_ps - 0.47, sprintf("Spannweite %s, %d-mal so groß", zahl_de(streu_ps), L - 1),
        hjust = 0.5, colour = dunkel) +
  geom_point(aes(loo, y_loo, fill = antwort, colour = antwort), shape = 21, size = 3.2, stroke = 0.8) +
  geom_point(aes(ps_ref, y_ps, fill = antwort, colour = antwort), shape = 21, size = 3.2, stroke = 0.8) +
  intervall(y_ref, p$jk_ref, p$se_ref, blau) +
  intervall(y_alt, p$jk_alt, p$se_alt, akzent) +
  intervall(y_mod, p$theta, p$se_modell, grau) +
  notiz(p$theta, 4.75, sprintf("θ̂ = %s", zahl_de(p$theta)), hjust = -0.08) +
  notiz(x_rechts, (y_loo + y_ps) / 2,
        sprintf(paste0("Alle Linien laufen durch θ̂: ein Hebel,\nder jeden Abstand zu θ̂ %d-fach verlängert.\n",
                       "Ohne ein gelöstes Item sinkt θ̂,\nder Pseudowert steigt."), L - 1)) +
  notiz(x_rechts, y_alt,
        sprintf("PP 0.6.4-1 setzt die Streuung der\nLeave-one-out-Schätzer in die Formel\nfür Pseudowerte ein: SE um den\nFaktor %d zu klein.",
                L - 1), colour = akzent) +
  punkte_skala +
  scale_y_continuous(breaks = c(y_loo, y_ps, y_ref, y_alt, y_mod), labels = zeilen,
                     limits = c(0.5, 4.9)) +
  scale_x_continuous(breaks = seq(-4, 2, 1), labels = function(v) zahl_de(v, 0)) +
  coord_cartesian(xlim = c(min(it$ps_ref), x_rechts + 2.6)) +
  labs(
    title = "Der Jackknife misst die Streuung der Pseudowerte",
    subtitle = p$beschreibung,
    x = "θ", y = NULL,
    caption = stand
  ) +
  zeilen_theme
speichere_plot(p_pr, sprintf("jk_prinzip_%s", args$label), width = 12, height = 6.4, dir = ordner)

# --- jk_faktor --------------------------------------------------------------------

fa <- melt(d$faktor, id.vars = c("L", "person", "se_modell"), measure.vars = c("se_alt", "se_ref"),
           variable.name = "version", value.name = "se")
fa[, `:=`(version = factor(fifelse(version == "se_alt", "PP 0.6.4-1", "Lehrbuch"), levels = names(farben)),
          verhaeltnis = se / se_modell)]
median_fa <- fa[, .(verhaeltnis = median(verhaeltnis)), by = .(L, version)]
laengen <- sort(unique(fa$L))
x_text <- max(laengen) * 1.2
kurve <- data.table(L = exp(seq(log(min(laengen) * 0.85), log(max(laengen) * 1.12), length.out = 100)))
kurve[, v := 1 / (L - 1)]
set.seed(1)
fa[, x := L * exp(runif(.N, -0.09, 0.09))]
brueche <- c(1, 1 / 2, 1 / 5, 1 / 10, 1 / 20, 1 / 50)
n_pers <- range(d$faktor[, .N, by = L]$N)

p_fa <- ggplot(fa) +
  annotate("segment", x = min(laengen) * 0.8, xend = max(laengen) * 1.12, y = 1, yend = 1, colour = grau,
           linewidth = 0.4) +
  geom_line(data = kurve, aes(L, v), colour = akzent, linetype = "22", linewidth = 0.5) +
  geom_point(aes(x, verhaeltnis, colour = version), size = 1.6, alpha = 0.45) +
  geom_line(data = median_fa, aes(L, verhaeltnis, colour = version), linewidth = 1) +
  geom_point(data = median_fa, aes(L, verhaeltnis, colour = version), size = 3.2) +
  geom_text(data = median_fa[version == "PP 0.6.4-1"], aes(L * 0.87, verhaeltnis, label = sprintf("1/%s", round(1 / verhaeltnis))),
            colour = akzent, family = schrift, size = 3.4, fontface = "bold", hjust = 1) +
  notiz(x_text, 1, "Modell-SE (WLE)", colour = grau) +
  notiz(x_text, 1 / (max(laengen) * 1.12 - 1), "1 / (L − 1)", colour = akzent) +
  scale_colour_manual(values = farben, name = NULL,
                      labels = c("PP 0.6.4-1", "Lehrbuch-Jackknife")) +
  scale_x_log10(breaks = laengen, limits = c(min(laengen) * 0.75, max(laengen) * 1.75)) +
  scale_y_log10(breaks = brueche, labels = c("1", "1/2", "1/5", "1/10", "1/20", "1/50")) +
  coord_cartesian(ylim = c(1 / 70, 2.2)) +
  labs(
    title = "Je länger der Test, desto kleiner die alte Jackknife-SE",
    subtitle = sprintf("JK-SE geteilt durch die WLE-SE, 2PL, %d bis %d Personen je Testlänge · Linie: Median",
                       n_pers[1], n_pers[2]),
    x = "Zahl der Items L", y = "JK-SE / Modell-SE",
    caption = stand
  ) +
  theme_blog()
speichere_plot(p_fa, sprintf("jk_faktor_%s", args$label), width = 9, height = 5.6, dir = ordner)

# --- jk_fehlende_antworten ---------------------------------------------------------

n <- d$na
it <- copy(n$items)[, antwort := antwort(x)]
bearbeitet <- it[!is.na(x)]
k <- sum(is.na(it$x))
y_loo <- 4; y_alt_ps <- 3; y_ref_ps <- 2.1; y_ref <- 1.3; y_alt <- 0.8
zeilen <- c(
  "Leave-one-out-Schätzer",
  sprintf("Pseudowerte PP 0.6.4-1, alle %d Items\n%d · θ̂ − %d · (θ̂ ohne Item j)", n$L_alt, n$L_alt, n$L_alt - 1),
  sprintf("Pseudowerte Lehrbuch, %d bekommene\n%d · θ̂ − %d · (θ̂ ohne Item j)", n$L, n$L, n$L - 1),
  "Lehrbuch: Mittel ± SE",
  "PP 0.6.4-1: Mittel ± SE"
)
# the unanswered items: leaving one out changes nothing, so their
# leave-one-out estimate and their old pseudovalue are both theta
leer <- data.table(x = n$theta, y = c(y_loo, y_alt_ps) + 0.2, antwort = antwort(NA))
x_lo <- min(c(bearbeitet$ps_alt, n$jk_ref - n$se_ref))
x_hi <- max(bearbeitet$ps_alt)

p_na <- ggplot() +
  geom_vline(xintercept = n$theta, colour = grau, linetype = "22", linewidth = 0.4) +
  geom_point(data = leer, aes(x, y, fill = antwort, colour = antwort), shape = 21, size = 3.2, stroke = 0.8) +
  geom_text(data = leer, aes(x + 0.12, y, label = sprintf("%d ×", k)), colour = grau, size = 3.3,
            family = schrift, fontface = "bold", hjust = 0) +
  geom_point(data = bearbeitet, aes(loo, y_loo, fill = antwort, colour = antwort), shape = 21, size = 3.2,
             stroke = 0.8) +
  geom_point(data = bearbeitet, aes(ps_alt, y_alt_ps, fill = antwort, colour = antwort), shape = 21,
             size = 3.2, stroke = 0.8) +
  geom_point(data = bearbeitet, aes(ps_ref, y_ref_ps, fill = antwort, colour = antwort), shape = 21,
             size = 3.2, stroke = 0.8) +
  notiz(n$theta - 0.3, (y_loo + y_alt_ps) / 2 + 0.1,
        sprintf("%d Items hat die Person nicht bekommen.\nSie wegzulassen ändert nichts: PP 0.6.4-1\nzählt sie als %d Pseudowerte = θ̂ mit.", k, k),
        colour = grau, hjust = 1, box = TRUE) +
  notiz(n$theta, 4.75, sprintf("θ̂ = %s", zahl_de(n$theta)), hjust = -0.08) +
  intervall(y_ref, n$jk_ref, n$se_ref, blau) +
  intervall(y_alt, n$jk_alt, n$se_alt, akzent) +
  notiz(n$jk_alt + n$se_alt + 1.2, y_alt,
        sprintf("Das Mittel enthält %d-mal θ̂, die SE ist zusätzlich\num den Faktor %d zu klein.", k, n$L_alt - 1),
        colour = akzent) +
  punkte_skala +
  scale_y_continuous(breaks = c(y_loo, y_alt_ps, y_ref_ps, y_ref, y_alt), labels = zeilen,
                     limits = c(0.55, 4.85)) +
  scale_x_continuous(breaks = seq(-10, 10, 2), labels = function(v) zahl_de(v, 0)) +
  coord_cartesian(xlim = c(x_lo, x_hi)) +
  labs(
    title = "Nicht bearbeitete Items zählen im alten Jackknife mit",
    subtitle = n$beschreibung,
    x = "θ", y = NULL,
    caption = stand
  ) +
  zeilen_theme
speichere_plot(p_na, sprintf("jk_fehlende_antworten_%s", args$label), width = 12, height = 6.4, dir = ordner)

# --- jk_extremer_ausfall -------------------------------------------------------------

e <- d$extrem
it <- copy(e$items)[, antwort := antwort(x)]
endlich <- it[is.finite(loo)]
weg <- it[!is.finite(loo)]
y_loo <- 3.4; y_ps <- 2.5; y_alt <- 1.5; y_mod <- 0.9
x_lo <- e$theta - e$se_modell - 0.4
x_hi <- max(endlich$ps_ref) + 0.9
zeilen <- c("Leave-one-out-Schätzer", sprintf("Pseudowerte\n%d · θ̂ − %d · (θ̂ ohne Item j)", e$L, e$L - 1),
            "PP 0.6.4-1: Mittel ± SE", "MLE ± SE aus dem Modell")
pfeil <- arrow(length = unit(0.18, "cm"), type = "closed")

p_ex <- ggplot(endlich) +
  geom_vline(xintercept = e$theta, colour = grau, linetype = "22", linewidth = 0.4) +
  geom_segment(aes(x = loo, xend = ps_ref, y = y_loo, yend = y_ps), colour = hell, linewidth = 0.5) +
  geom_point(aes(loo, y_loo, fill = antwort, colour = antwort), shape = 21, size = 3.2, stroke = 0.8) +
  geom_point(aes(ps_ref, y_ps, fill = antwort, colour = antwort), shape = 21, size = 3.2, stroke = 0.8) +
  # item 12: the leave-one-out MLE is +Inf, its pseudovalue -Inf
  annotate("segment", x = e$theta + 0.15, xend = x_hi - 0.05, y = y_loo, yend = y_loo, colour = akzent,
           linewidth = 0.6, linetype = "42", arrow = pfeil) +
  annotate("segment", x = e$theta - 0.15, xend = x_lo + 0.05, y = y_ps, yend = y_ps, colour = akzent,
           linewidth = 0.6, linetype = "42", arrow = pfeil) +
  notiz(x_hi - 0.05, y_loo + 0.22, sprintf("ohne Item %d: alles gelöst, MLE = ∞", weg$item), hjust = 1,
        colour = akzent) +
  notiz(x_lo + 0.05, y_ps + 0.22, "Pseudowert −∞", hjust = 0, colour = akzent) +
  notiz(e$theta, 3.85, sprintf("θ̂ = %s", zahl_de(e$theta)), hjust = -0.08) +
  intervall(y_alt, e$jk_alt, e$se_alt, akzent) +
  intervall(y_mod, e$theta, e$se_modell, grau) +
  notiz(e$jk_alt, y_alt - 0.3,
        sprintf("PP 0.6.4-1 lässt den unendlichen Wert ohne Warnung weg\nund mittelt die %d übrigen: JK-Schätzer %s statt θ̂ = %s.",
                nrow(endlich), zahl_de(e$jk_alt), zahl_de(e$theta)), hjust = 0, colour = akzent) +
  notiz(e$jk_alt + e$se_alt + 0.25, (y_ps + y_alt) / 2,
        "Lehrbuch: Ein Pseudowert ist unendlich,\nder Jackknife hat hier kein endliches Ergebnis.",
        colour = blau, box = TRUE) +
  punkte_skala +
  scale_y_continuous(breaks = c(y_loo, y_ps, y_alt, y_mod), labels = zeilen, limits = c(0.55, 3.95)) +
  scale_x_continuous(labels = function(v) zahl_de(v, 0)) +
  coord_cartesian(xlim = c(x_lo, x_hi)) +
  labs(
    title = "Ein unendlicher Wert verschwindet still",
    subtitle = e$beschreibung,
    x = "θ", y = NULL,
    caption = stand
  ) +
  zeilen_theme
speichere_plot(p_ex, sprintf("jk_extremer_ausfall_%s", args$label), width = 11, height = 5.6, dir = ordner)

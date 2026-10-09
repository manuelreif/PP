# Data flow of the input handling before and after the one-input-layer step:
# before, PP_4pl, PP_gpcm, PPall (and findmodel) each prepared their inputs
# themselves; after, all go through pp_prepare(), which hands a normalised
# object to the (still unchanged) old estimation code.
#
#   Rscript blog/datenfluss.R [--label=block2]
#
# Writes blog/output/datenfluss_<label>.png/.svg. A structure diagram, no data.

suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})
source("blog/gemeinsam.R")
args <- blog_args()
stand <- sprintf("Stand: %s.", gsub("`", "", code_stand()))

dunkel <- "#1F2328"
blau <- "#0969DA"
stil <- list(
  fkt  = c(fill = "white",   rand = dunkel),
  alt  = c(fill = "#FFEDD5", rand = akzent),
  neu  = c(fill = "#DDF4FF", rand = blau),
  paket = c(fill = "white",  rand = blau),
  kern = c(fill = "#F6F8FA", rand = "#8C959F")
)

# A box: centre (x, y), width w, height h, bold title and optional body text
box <- function(x, y, w, h, titel, text = "", art, lh = 1.05) {
  data.table(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
             x = x, y = y, h = h, titel = titel, text = text, art = art, lh = lh)
}

zeichne <- function(boxen, pfeile, xlim, ylim, ueberschrift) {
  boxen[, fill := vapply(art, function(a) stil[[a]][["fill"]], "")]
  boxen[, rand := vapply(art, function(a) stil[[a]][["rand"]], "")]
  mit_text <- boxen[text != ""]
  ohne_text <- boxen[text == ""]
  ggplot() +
    geom_segment(data = pfeile[spitze == FALSE], aes(x = x, y = y, xend = xend, yend = yend, linetype = typ),
                 colour = grau, linewidth = 0.5) +
    geom_segment(data = pfeile[spitze == TRUE], aes(x = x, y = y, xend = xend, yend = yend, linetype = typ),
                 colour = grau, linewidth = 0.5,
                 arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
    geom_rect(data = boxen, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = boxen$fill, colour = boxen$rand, linewidth = 0.6) +
    geom_text(data = ohne_text, aes(x, y, label = titel), fontface = "bold", size = 3.6,
              colour = dunkel, family = "Open Sans") +
    geom_text(data = mit_text, aes(x, ymax - 0.16, label = titel), fontface = "bold", size = 3.4,
              colour = dunkel, family = "Open Sans", vjust = 1) +
    geom_text(data = mit_text, aes(x, ymax - 0.42, label = text, lineheight = lh), size = 3.1,
              colour = dunkel, family = "Open Sans", vjust = 1) +
    geom_text(data = pfeile[label != ""], aes((x + xend) / 2, (y + yend) / 2, label = label),
              size = 3, colour = grau, family = "Open Sans", hjust = 0.5, vjust = -0.6) +
    scale_linetype_manual(values = c(fluss = "solid", nutzt = "22"), guide = "none") +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = ueberschrift) +
    theme_void(base_family = "Open Sans") +
    theme(plot.title = element_text(face = "bold", size = 13, colour = dunkel,
                                    margin = margin(b = 4)))
}

pfeil <- function(x, y, xend, yend, typ = "fluss", label = "", spitze = TRUE) {
  data.table(x = x, y = y, xend = xend, yend = yend, typ = typ, label = label, spitze = spitze)
}

wrapper <- c("PP_4pl()", "PP_gpcm()", "PPall()")
kern <- c("NR_4PL · eap_4pl", "NR_GPCM · eap_gpcm", "NR_mixed · eap_mixed")
zeilen <- c(3, 2, 1)
xw <- 8; ww <- 14          # wrapper column
xk <- 89; wk <- 22         # estimation code column

# --- before -------------------------------------------------------------------

vorbereitung <- c(
  "ctrl · thres · Startwerte · checkINP() · Prior-Defaults\nModell raten · Längen prüfen · Parameter auffüllen",
  "ctrl · thres · Startwerte · checkINP() · Prior-Defaults\n(slopes ohne Default)",
  "ctrl · thres (nur als Matrix) · Startwerte · checkINP()\nmodel2est · Prior-Defaults · Asymptoten-Warnung"
)
xv <- 45; wv <- 50

b_vorher <- rbind(
  box(xw, zeilen, ww, 0.5, wrapper, art = "fkt"),
  box(xv, zeilen, wv, 0.82, "eigene Vorbereitung", vorbereitung, art = "alt"),
  box(xk, zeilen, wk, 0.5, kern, art = "kern"),
  box(xw, 0, ww, 0.5, "findmodel()", art = "fkt"),
  box(xv, 0, wv, 0.6, "eigene Vorbereitung", "thres (vierte Kopie)", art = "alt")
)
p_vorher <- rbind(
  pfeil(xw + ww / 2, c(zeilen, 0), xv - wv / 2, c(zeilen, 0)),
  pfeil(xv + wv / 2, zeilen, xk - wk / 2, zeilen)
)
g_vorher <- zeichne(b_vorher, p_vorher, c(0, 100.5), c(-0.45, 3.5),
                    "Vorher: jede Funktion bereitet ihre Eingaben selbst vor")

# --- after --------------------------------------------------------------------

xp <- 38; wp <- 26         # pp_prepare()
xo <- 64; wo <- 16         # prep object
b_nachher <- rbind(
  box(xw, zeilen, ww, 0.5, wrapper, art = "fkt"),
  box(xp, 2, wp, 2.5, "pp_prepare()",
      "family = \"4pl\" | \"gpcm\" | \"mixed\"\n\nnormalize_thres()\nDefaults und Recycling\nPrüfungen\nItembank bauen",
      art = "neu", lh = 1.45),
  box(xo, 2, wo, 2.5, "prep",
      "X\nthres, maxsc\nslopes, lowerA, upperA\nmu, sigma2, start\ntype, ctrl …\nbank",
      art = "paket", lh = 1.45),
  box(xk, zeilen, wk, 0.5, kern, art = "kern"),
  box(xw, 0, ww, 0.5, "findmodel()", art = "fkt")
)
p_nachher <- rbind(
  pfeil(xw + ww / 2, zeilen, xp - wp / 2, zeilen),
  pfeil(xp + wp / 2, 2, xo - wo / 2, 2),
  pfeil(xo + wo / 2, 2, xk - wk / 2, zeilen),
  # findmodel() uses the same thres rule: dashed line to the bottom of pp_prepare()
  pfeil(xw + ww / 2, 0, xp, 0, typ = "nutzt", label = "nutzt dieselbe thres-Regel", spitze = FALSE),
  pfeil(xp, 0, xp, 2 - 2.5 / 2, typ = "nutzt")
)
g_nachher <- zeichne(b_nachher, p_nachher, c(0, 100.5), c(-0.45, 3.5),
                     "Nachher: eine Eingabeschicht für alle")

# --- legend and assembly ------------------------------------------------------

legende <- rbind(
  box(6, 0, 10, 0.5, "", art = "alt"),
  box(36, 0, 10, 0.5, "", art = "neu"),
  box(66, 0, 10, 0.5, "", art = "kern")
)
legende[, text := ""]
g_legende <- zeichne(legende, pfeil(0, 0, 0, 0)[0], c(0, 100.5), c(-0.4, 0.4), NULL) +
  annotate("text", x = c(12, 42, 72), y = 0, hjust = 0, size = 3.2, colour = dunkel,
           family = "Open Sans",
           label = c("kopierter Vorbereitungscode", "neu: Eingabeschicht",
                     "Schätzcode (noch unverändert)"))

p <- (g_vorher / g_nachher / g_legende) +
  plot_layout(heights = c(4, 4, 0.5)) +
  plot_annotation(
    title = "Eine Tür für alle Eingaben",
    subtitle = "Wie die Schätzfunktionen ihre Eingaben aufbereiten, vor und nach dem Umbau",
    caption = paste0("prep ist eine Liste mit den geprüften, vollständig ausgefüllten Eingaben; ",
                     "bank beschreibt alle Items in einer gemeinsamen Form.\n", stand),
    theme = theme_blog()
  )
speichere_plot(p, sprintf("datenfluss_%s", args$label), width = 10, height = 8)

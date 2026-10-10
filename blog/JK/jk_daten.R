# Data for the pictures of the jackknife: JKpp of PP 0.6.4-1 against the
# textbook jackknife (ref_jackknife in tests/testthat/helper-2-reference.R).
#
#   Rscript blog/JK/jk_daten.R [--label=block5]
#
# Writes blog/JK/output/jk_<label>.rds, a list with
#   prinzip  one person (2PL, 12 items, WLE): leave-one-out estimates,
#            pseudovalues, JK estimate and SE from JKpp and the textbook
#   faktor   JK-SE / WLE-SE for 2PL tests with 6 to 48 items
#   na       one person who got every second item only (6 of 12, WLE)
#   extrem   one person with a single wrong answer (MLE): without that item
#            the MLE does not exist
#
# R/JKpp.R is unchanged since PP 0.6.4-1, so the installed JKpp is the old
# code. Run from the package root against the installed PP.

source("blog/gemeinsam.R")
lade_test_helfer()
args <- blog_args()
ordner <- "blog/JK/output"

jk_alt <- function(obj) pp_quiet(JKpp(obj, fullmat = TRUE))

# Leave-one-out estimates and pseudovalues of one person, from both versions.
# The old JKpp fails for a single person, so the pattern is fitted twice.
person_daten <- function(x, items, type) {
  obj <- pp_fit(list(items = items, kind = "4pl"), type, X = unname(rbind(x, x)))
  jk <- jk_alt(obj)
  ref <- ref_jackknife(x, items, type)
  L_alt <- length(x)
  theta <- obj$estimates$estimate[1]
  pro_item <- data.table(item = seq_along(x), x = x, loo = ref$loo, loo_alt = jk$jk_mat[1, ])
  pro_item[, `:=`(ps_alt = L_alt * theta - (L_alt - 1) * loo_alt,
                  ps_ref = sum(!is.na(x)) * ref$theta - (sum(!is.na(x)) - 1) * loo)]
  list(items = pro_item, theta = theta, se_modell = obj$estimates$SE[1], theta_ref = ref$theta,
       L = sum(!is.na(x)), L_alt = L_alt,
       jk_alt = jk$resjk[1, 1], se_alt = jk$resjk[1, 2],
       jk_ref = ref$estimate, se_ref = ref$se)
}

# --- one person: from leave-one-out estimates to pseudovalues -----------------

s <- set_2pl
x1 <- s$X[1, ]
prinzip <- person_daten(x1, s$items, "wle")
prinzip$beschreibung <- "2PL, 12 Items, eine Person mit 5 gelösten Items, WLE"

# --- JK-SE relative to the model SE over the test length -----------------------

laengen <- c(6, 12, 24, 48)
set.seed(6)
theta_p <- rnorm(40)
faktor <- rbindlist(lapply(laengen, function(L) {
  set.seed(600 + L)
  items <- items_4pl(round(runif(L, 0.8, 1.6), 2), round(seq(-2, 2, length.out = L), 2))
  X <- sim_resp(theta_p, items, 700 + L)
  X <- X[!apply(X, 1, ref_is_extreme, items = items), , drop = FALSE]
  obj <- pp_fit(list(items = items, kind = "4pl"), "wle", X = X)
  jk <- jk_alt(obj)
  ref <- vapply(seq_len(nrow(X)), function(i) ref_jackknife(X[i, ], items, "wle")$se, 0)
  data.table(L = L, person = seq_len(nrow(X)), se_modell = obj$estimates$SE,
             se_alt = jk$resjk[, 2], se_ref = ref)
}))

# --- missing responses: every second item answered ----------------------------

x_na <- x1
x_na[seq(1, 11, by = 2)] <- NA
na <- person_daten(x_na, s$items, "wle")
na$beschreibung <- "2PL, 12 Items, die Person hat nur jedes zweite Item bekommen (wie in einem Testheft), WLE"

# --- an extreme leave-one-out sample with MLE ----------------------------------

x_ex <- c(rep(1L, 11), 0L)
extrem <- person_daten(x_ex, s$items, "mle")
extrem$beschreibung <- "2PL, 12 Items, die Person hat nur Item 12 falsch, MLE"

dir.create(ordner, showWarnings = FALSE, recursive = TRUE)
out <- file.path(ordner, sprintf("jk_%s.rds", args$label))
saveRDS(list(prinzip = prinzip, faktor = faktor, na = na, extrem = extrem,
             code = code_stand(), zeitpunkt = Sys.time()), out)
message("gespeichert: ", out)

kurz <- function(p) unlist(p[c("theta", "se_modell", "jk_alt", "se_alt", "jk_ref", "se_ref", "L")])
print(rbind(prinzip = kurz(prinzip), na = kurz(na), extrem = kurz(extrem)))
print(faktor[, .(n = .N, alt = median(se_alt / se_modell), ref = median(se_ref / se_modell),
                 alt_ref = median(se_alt / se_ref)), by = L])
print(extrem$items)

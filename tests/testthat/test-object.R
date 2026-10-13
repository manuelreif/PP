# The result object of PP_4pl, PP_gpcm and PPall: $estimates with status, the
# alias $resPP (the transition field $ipar is gone since block 7), the methods,
# the warning for persons at the bound, and PV, JKpp and Pfit on the object.

test_that("the object has estimates with status, model, type, call and the alias", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    for (ty in c("mle", "wle", "map", "eap", "robust")) {
      o <- pp_fit(s, ty)
      cls <- switch(s$kind, "4pl" = "fourpl", gpcm = "gpcm", mixed = "gpcm4pl")
      expect_s3_class(o, c(cls, "ppeo"), exact = TRUE)
      e <- o$estimates
      expect_s3_class(e, "data.frame")
      expect_identical(names(e), c("estimate", "SE", "nsteps", "status"))
      expect_identical(nrow(e), nrow(s$X))
      expect_identical(levels(e$status),
                       c("ok", "extreme", "boundary", "no_response", "not_converged"))
      expect_type(e$nsteps, "integer")
      expect_identical(o$type, ty)
      expect_identical(o$model$X, `storage.mode<-`(s$X, "integer"))
      expect_true(is.call(o$call))
      # alias in the layout of 0.6.4
      expect_identical(unname(o$resPP$resPP), unname(as.matrix(e[, 1:2])))
      expect_identical(colnames(o$resPP$resPP), c("estimate", "SE"))
      expect_identical(o$resPP$nsteps, max(e$nsteps))
      if (ty == "eap") expect_true(all(e$nsteps == 0L))
    }
  }
})

test_that("the status marks extreme scores, persons without answers and the bound", {
  X <- set_2pl$X
  X[2, ] <- NA
  o <- pp_fit(set_2pl, "mle", X = X)
  ext <- apply(set_2pl$X, 1, ref_is_extreme, items = set_2pl$items) != 0
  expect_identical(as.character(o$estimates$status[2]), "no_response")
  expect_true(all(o$estimates$status[setdiff(which(ext), 2)] == "extreme"))
  expect_true(all(o$estimates$status[-c(2, which(ext))] == "ok"))
  # wle has finite estimates for the extreme scores
  w <- pp_fit(set_2pl, "wle", X = X)
  expect_true(all(w$estimates$status[-2] == "ok"))
  # the 4PL set has a person without a finite mle
  m <- pp_fit(set_4pl, "mle")
  expect_true(any(m$estimates$status == "boundary"))
  expect_true(all(abs(m$estimates$estimate[m$estimates$status == "boundary"]) == 6))
})

test_that("persons at the bound or not converged give one warning each", {
  p <- items_to_pp(set_4pl$items)
  fit <- function(...) PP_4pl(set_4pl$X, p$b, p$slopes, p$lowerA, p$upperA, type = "mle", ...)
  expect_warning(fit(), "1 person\\(s\\) without a solution inside 'range' = c\\(-6, 6\\)")
  expect_warning(fit(range = c(-3, 3)), "status \"boundary\"")
  expect_warning(PP_4pl(set_3pl$X, p$b, p$slopes, p$lowerA, type = "wle", exac = 1e-12,
                        maxsteps = 2), "not converged within maxsteps = 2")
  p2 <- items_to_pp(set_2pl$items)
  expect_silent(PP_4pl(set_2pl$X, p2$b, p2$slopes, type = "wle"))
})

test_that("verbose = TRUE gives a message, the default is silent", {
  p <- items_to_pp(set_2pl$items)
  expect_message(PP_4pl(set_2pl$X, p$b, p$slopes, verbose = TRUE), "2pl model, type = wle")
  pm <- items_to_pp(set_mixed$items)
  expect_message(PPall(set_mixed$X, pm$thres, pm$slopes, verbose = TRUE),
                 "mixed")
  expect_silent(PPall(set_mixed$X, pm$thres, pm$slopes))
  expect_error(PP_4pl(set_2pl$X, p$b, verbose = NA), "'verbose' must be TRUE or FALSE")
})

test_that("range is checked and used", {
  p <- items_to_pp(set_2pl$items)
  for (r in list(c(1, -1), 1, c(-Inf, 6), c("a", "b"))) {
    expect_error(PP_4pl(set_2pl$X, p$b, range = r), "'range' must be")
  }
  o <- suppressWarnings(PP_4pl(set_2pl$X, p$b, p$slopes, type = "map", range = c(-0.5, 0.5)))
  expect_true(all(abs(o$estimates$estimate) <= 0.5))
  expect_true(any(o$estimates$status == "boundary"))
})

test_that("the transition field $ipar of 0.6.4 is gone", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) expect_null(pp_fit(s, "wle")$ipar)
  expect_named(pp_fit(set_2pl, "mle"), c("estimates", "type", "model", "call", "resPP"))
})

test_that("F7: row names of the responses are not carried over, duplicates work", {
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    X <- s$X
    rownames(X) <- rep(c("a", "b"), length.out = nrow(X))
    ref <- pp_fit(s, "map")
    o <- pp_fit(s, "map", X = X)
    expect_identical(o$estimates, ref$estimates)
    expect_identical(rownames(o$estimates), as.character(seq_len(nrow(X))))
    expect_null(rownames(o$resPP$resPP))
    expect_null(names(coef(o)))
    expect_identical(pp_quiet(JKpp(o))$estimates, pp_quiet(JKpp(ref))$estimates)
    expect_identical(Pfit(pp = o)[1:4], Pfit(pp = ref)[1:4])
    set.seed(1); a <- PV(o, npv = 2)$pvdraws
    set.seed(1); b <- PV(ref, npv = 2)$pvdraws
    expect_identical(a, b)
  }
  p2 <- items_to_pp(set_2pl$items)
  d <- data.frame(id = rep(1:2, length.out = nrow(set_2pl$X)), set_2pl$X)
  rownames(d) <- paste0("r", seq_len(nrow(d)))
  res <- pp_quiet(PPass(d, items = 2:13, thres = p2$b, slopes = p2$slopes))
  expect_identical(rownames(res), as.character(seq_len(nrow(d))))
  expect_identical(res$id, d$id)
})

test_that("print, summary, coef and as.data.frame work", {
  X <- set_2pl$X
  X[2, ] <- NA
  o <- pp_fit(set_2pl, "mle", X = X)
  expect_output(print(o), "estimate")
  capture.output(r <- withVisible(print(o)))
  expect_false(r$visible)
  expect_identical(r$value, o)
  out <- capture.output(summary(o, nrowmax = 5))
  expect_true(any(grepl("no_response", out)))
  expect_true(any(grepl("output truncated", out)))
  expect_identical(coef(o), o$estimates$estimate)
  expect_identical(as.data.frame(o), o$estimates)
})

test_that("PV, JKpp and Pfit run on the object", {
  jk <- function(o) pp_quiet(suppressWarnings(JKpp(o)))
  for (s in list(set_2pl, set_gpcm, set_mixed)) {
    for (ty in c("mle", "wle", "map", "eap")) {
      o <- pp_fit(s, ty)
      j <- jk(o)
      expect_identical(dim(j$resjk), c(nrow(s$X), 2L))
      # rows without a solution (extreme mle) have no jackknife
      na <- o$estimates$status != "ok"
      expect_true(all(is.na(j$resjk[na, ])))
      expect_identical(as.character(j$estimates$status[na]), as.character(o$estimates$status[na]))
      expect_false(anyNA(j$resjk[!na, ]))
      # PV needs a prior (B6): mle and wle objects have none
      if (ty %in% c("mle", "wle")) {
        expect_true(all(is.na(o$model$mu)) && all(is.na(o$model$sigma2)))
        expect_error(PV(o), "prior")
        next
      }
      set.seed(1)
      pv <- PV(o, npv = 3, approx = TRUE)$pvdraws
      expect_identical(dim(pv), c(nrow(s$X), 3L))
      expect_false(anyNA(pv))
    }
  }
  o <- pp_fit(set_2pl, "wle")
  pf <- pp_quiet(Pfit(set_2pl$X, o, fitindices = c("lz", "infit")))
  expect_identical(nrow(pf$lz), nrow(set_2pl$X))
})

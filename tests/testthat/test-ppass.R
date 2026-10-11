# PPass: estimation and person fit in one call, routed to PP_4pl / PP_gpcm /
# PPall. The eRm method is tested in test-erm.R.

p2 <- items_to_pp(set_2pl$items)
df2 <- data.frame(id = seq_len(nrow(set_2pl$X)), set_2pl$X)
ppass_2pl <- function(...) {
  pp_quiet(PPass(df2, items = 2:13, mod = "2PL", thres = p2$b, slopes = p2$slopes, ...))
}

test_that("PPass with mod = '2PL' equals PP_4pl plus Pfit", {
  res <- ppass_2pl(type = "wle", fitindices = c("lz", "infit"))
  obj <- pp_fit(set_2pl, "wle")
  pf <- pp_quiet(Pfit(set_2pl$X, obj, fitindices = c("lz", "infit")))
  expect_equal(unname(as.matrix(res[, c("estimate", "SE")])), unname(est(obj)))
  expect_identical(res$status, obj$estimates$status)
  expect_equal(res$lz, unname(pf$lz[, "lz"]))
  expect_equal(res$infit, unname(pf$infit[, "infit"]))
})

test_that("PPass with mod = 'PCM' equals PP_gpcm plus Pfit", {
  pp <- items_to_pp(set_pcm$items)
  dfp <- data.frame(id = seq_len(nrow(set_pcm$X)), set_pcm$X)
  res <- pp_quiet(PPass(dfp, items = 2:7, mod = "PCM", thres = pp$thres, slopes = pp$slopes,
                        type = "wle", fitindices = "infit"))
  obj <- pp_fit(set_pcm, "wle")
  pf <- pp_quiet(Pfit(set_pcm$X, obj, fitindices = "infit"))
  expect_equal(unname(as.matrix(res[, c("estimate", "SE")])), unname(est(obj)))
  expect_equal(res$infit, unname(pf$infit[, "infit"]))
})

test_that("G1: PPass works without mod (default 1PL)", {
  expect_equal(as.data.frame(pp_quiet(PPass(df2, items = 2:13, thres = p2$b, fitindices = "lz"))),
               as.data.frame(pp_quiet(PPass(df2, items = 2:13, mod = "1PL", thres = p2$b,
                                            fitindices = "lz"))))
})

test_that("G1: PPass works for mixed tests", {
  pm <- items_to_pp(set_mixed$items)
  dfm <- data.frame(id = seq_len(nrow(set_mixed$X)), set_mixed$X)
  res <- pp_quiet(PPass(dfm, items = 2:13, mod = "MIXED", thres = pm$thres, slopes = pm$slopes,
                        lowerA = pm$lowerA, upperA = pm$upperA, model2est = pm$model2est,
                        type = "wle", fitindices = "infit"))
  obj <- pp_fit(set_mixed, "wle")
  expect_equal(unname(as.matrix(res[, c("estimate", "SE")])), unname(est(obj)))
  expect_equal(res$infit, unname(pp_quiet(Pfit(pp = obj, fitindices = "infit"))$infit[, "infit"]))
})

test_that("G1: PPass returns the non-item columns", {
  res <- ppass_2pl(type = "wle", fitindices = "lz")
  expect_identical(res$id, df2$id)
  expect_identical(names(res)[1:4], c("id", "estimate", "SE", "status"))
})

test_that("G1: PPass accepts a matrix", {
  a <- pp_quiet(PPass(as.matrix(df2), items = 2:13, mod = "2PL", thres = p2$b,
                      slopes = p2$slopes, fitindices = "lz"))
  b <- ppass_2pl(fitindices = "lz")
  expect_identical(names(a), names(b))
  expect_equal(as.data.frame(a), as.data.frame(b), ignore_attr = TRUE)
})

test_that("items can be given by name", {
  res <- pp_quiet(PPass(df2, items = names(df2)[-1], mod = "2PL", thres = p2$b,
                        slopes = p2$slopes, fitindices = "lz"))
  expect_equal(as.data.frame(res), as.data.frame(ppass_2pl(fitindices = "lz")))
  expect_error(PPass(df2, items = c("V1", "nope"), thres = p2$b), "nope")
  expect_error(PPass(df2, items = c(2, 2), thres = p2$b), "distinct")
  expect_error(PPass(df2, items = 0:3, thres = p2$b), "'items'")
  expect_error(PPass(list(1), thres = 1), "data.frame or a matrix")
})

test_that("mod must fit the item parameters", {
  expect_error(PPass(df2, items = 2:13, mod = "1PL", thres = p2$b, slopes = p2$slopes),
               "mod = '1PL'.*'slopes'")
  expect_error(PPass(df2, items = 2:13, mod = "2PL", thres = p2$b), "mod = '2PL'.*'slopes'")
  expect_error(PPass(df2, items = 2:13, mod = "3PL", thres = p2$b, slopes = p2$slopes),
               "mod = '3PL'.*'lowerA'")
  expect_error(PPass(df2, items = 2:13, mod = "4PL", thres = p2$b, lowerA = 0.1),
               "mod = '4PL'.*'upperA'")
  pp <- items_to_pp(set_pcm$items)
  expect_error(PPass(set_pcm$X, mod = "PCM", thres = pp$thres, slopes = rep(1.2, 6)),
               "mod = 'PCM'")
  expect_error(PPass(df2, items = 2:13, mod = "Rasch", thres = p2$b), "'arg'")
})

test_that("eap and robust give the estimates only, with a message", {
  expect_message(res <- ppass_2pl(type = "eap"), "returns the eap estimates only")
  expect_identical(names(res), c("id", "estimate", "SE", "status"))
  expect_equal(unname(as.matrix(res[, c("estimate", "SE")])), unname(est(pp_fit(set_2pl, "eap"))))
  expect_identical(attr(res, "info")$fitindices, character(0))
})

test_that("SE = TRUE adds the jackknife SEs of Pfit", {
  res <- ppass_2pl(fitindices = c("lz", "outfit"), SE = TRUE)
  pf <- pp_quiet(Pfit(pp = pp_fit(set_2pl, "wle"), fitindices = c("lz", "outfit"), SE = TRUE))
  expect_equal(res$lz_se, unname(pf$lz[, "lz_se"]))
  expect_equal(res$ou_se, unname(pf$outfit[, "ou_se"]))
})

test_that("the result is a data.frame of class ppass with the estimation object", {
  res <- ppass_2pl()
  expect_s3_class(res, c("ppass", "data.frame"), exact = TRUE)
  expect_s3_class(attr(res, "ppeo"), "ppeo")
  expect_identical(attr(res, "ppeo")$call[[1]], quote(PPass))
  plain <- as.data.frame(res)
  expect_identical(class(plain), "data.frame")
  expect_null(attr(plain, "ppeo"))
  expect_null(attr(plain, "info"))
  expect_identical(names(plain), names(res))
})

test_that("print and summary give an overview with the flagged persons", {
  res <- ppass_2pl()
  expect_output(print(res), "Person assessment: 20 persons, 12 items")
  expect_output(print(res), "Not shown: lz_unst")
  s <- NULL
  expect_output(s <- summary(res, alpha = 0.2), "Flagged \\(\\|lzstar\\| > 1.28\\)")
  expect_identical(s$fit$flagged[s$fit$statistic == "lzstar"], nrow(s$flagged))
  expect_true(all(abs(s$flagged$lzstar) > qnorm(0.9)))
  expect_identical(sum(s$status), nrow(res))
  expect_error(summary(res, alpha = 2), "'alpha'")
})

test_that("a subset prints as data.frame and JKpp needs the complete result", {
  res <- ppass_2pl()
  sub <- res[1:10, ]
  expect_false(any(grepl("Person assessment", capture.output(print(sub)))))
  expect_error(JKpp(sub), "not a complete result")
  expect_error(summary(sub), "not a complete result")
})

test_that("JKpp and PV work on the result directly", {
  res <- ppass_2pl(type = "map")
  obj <- attr(res, "ppeo")
  expect_identical(pp_quiet(JKpp(res))$estimates, pp_quiet(JKpp(obj))$estimates)
  set.seed(5); a <- PV(res, npv = 3)
  set.seed(5); b <- PV(obj, npv = 3)
  expect_identical(a$pvdraws, b$pvdraws)
})

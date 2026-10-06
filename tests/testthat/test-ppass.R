# PPass: estimation and person fit in one call, routed to PP_4pl / PP_gpcm /
# PPall.

p2 <- items_to_pp(set_2pl$items)
df2 <- data.frame(id = seq_len(nrow(set_2pl$X)), set_2pl$X)

test_that("PPass with mod = '2PL' equals PP_4pl plus Pfit", {
  res <- pp_quiet(PPass(df2, items = 2:13, mod = "2PL", thres = p2$b, slopes = p2$slopes,
                        type = "wle", fitindices = c("lz", "infit")))
  obj <- pp_fit(set_2pl, "wle")
  pf <- pp_quiet(Pfit(set_2pl$X, obj, fitindices = c("lz", "infit")))
  expect_equal(unname(as.matrix(res[, c("estimate", "SE")])), unname(est(obj)))
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

# --- known bugs -----------------------------------------------------------

test_that("G1: PPass works without mod (default 1PL)", {
  skip_known("G1", 8)
  expect_equal(pp_quiet(PPass(df2, items = 2:13, thres = p2$b, fitindices = "lz")),
               pp_quiet(PPass(df2, items = 2:13, mod = "1PL", thres = p2$b, fitindices = "lz")))
})

test_that("G1: PPass works for mixed tests", {
  skip_known("G1", 8)
  pm <- items_to_pp(set_mixed$items)
  dfm <- data.frame(id = seq_len(nrow(set_mixed$X)), set_mixed$X)
  res <- pp_quiet(PPass(dfm, items = 2:13, mod = "MIXED", thres = pm$thres, slopes = pm$slopes,
                        lowerA = pm$lowerA, upperA = pm$upperA, model2est = pm$model2est,
                        type = "wle", fitindices = "infit"))
  expect_equal(unname(as.matrix(res[, c("estimate", "SE")])), unname(est(pp_fit(set_mixed, "wle"))))
})

test_that("G1: PPass returns the non-item columns", {
  skip_known("G1", 8)
  res <- pp_quiet(PPass(df2, items = 2:13, mod = "2PL", thres = p2$b, slopes = p2$slopes,
                        type = "wle", fitindices = "lz"))
  expect_identical(res$id, df2$id)
})

test_that("G1: PPass accepts a matrix", {
  skip_known("G1", 8)
  expect_equal(pp_quiet(PPass(as.matrix(df2), items = 2:13, mod = "2PL", thres = p2$b,
                              slopes = p2$slopes, fitindices = "lz")),
               pp_quiet(PPass(df2, items = 2:13, mod = "2PL", thres = p2$b,
                              slopes = p2$slopes, fitindices = "lz")))
})

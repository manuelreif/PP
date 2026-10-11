# PPass on models fitted with eRm: the responses and item parameters come
# from the eRm object. Reference: eRm::person.parameter() (MLE) and, for group
# contrasts, which eRm does not estimate, a selection of the parameters by name.

skip_if_not_installed("eRm")

erm_data <- local({
  set.seed(4711)
  thres <- rbind(0, matrix(c(-1.5, 0, 1, -1, 0.5, NA, -0.5, 1, NA, 0, 0.8, NA, -2, -0.2, NA,
                             0.4, 1.2, NA), 3))
  poly <- function(n, shift = 0) sim_gpcm(thres = thres, alpha = rep(1, 6), theta = rnorm(n, shift))
  rsm <- function(n, shift = 0) {
    th <- rbind(0, outer(c(-0.8, 0.8), seq(-1, 1, length.out = 6), "+"))
    sim_gpcm(thres = th, alpha = rep(1, 6), theta = rnorm(n, shift))
  }
  dich <- function(n, shift = 0) sim_4pl(beta = seq(-1.5, 1.5, length.out = 8), alpha = rep(1, 8),
                                        lowerA = rep(0, 8), upperA = rep(1, 8), theta = rnorm(n, shift))
  # two patterns of missing responses (eRm estimates per pattern, tiny patterns break it)
  d <- dich(250)
  d[1:60, 7:8] <- NA
  out <- list(d = d, d2 = cbind(dich(250), dich(250, 0.5)), p = poly(250),
              p2 = cbind(poly(250), poly(250, 0.5)), r = rsm(250))
  # with two time points eRm::LRSM (fit or person.parameter) fails on data
  # simulated from a rating scale model; uniform random responses work
  out$r2 <- matrix(sample(0:2, 250 * 12, replace = TRUE), 250)
  out
})

erm_fits <- suppressWarnings(with(erm_data, list(
  RM = eRm::RM(d),
  LLTM = eRm::LLTM(d, W = cbind(seq(-1, 1, length.out = 8), rep(0:1, 4))),
  RSM = eRm::RSM(r),
  PCM = eRm::PCM(p),
  LRSM = eRm::LRSM(r),
  LPCM = eRm::LPCM(p),
  LLTM_2t = eRm::LLTM(d2, mpoints = 2),
  LRSM_2t = eRm::LRSM(r2, mpoints = 2),
  LPCM_2t = eRm::LPCM(p2, mpoints = 2)
)))

for (nm in names(erm_fits)) {
  test_that(paste("MLE of PPass equals eRm::person.parameter for", nm), {
    o <- erm_fits[[nm]]
    res <- pp_quiet(PPass(o, type = "mle", fitindices = "lzstar"))
    utils::capture.output(pt <- suppressWarnings(eRm::person.parameter(o))$theta.table)
    i <- match(rownames(o$X), rownames(pt))
    sel <- res$status == "ok" & !pt[i, "Interpolated"]
    expect_gt(sum(sel), 200)
    expect_close(res$estimate[sel], pt[i, 1][sel], 1e-4)
    expect_close(res$SE[sel], pt[i, 2][sel], 1e-3)
    expect_identical(nrow(res), nrow(o$X))
    expect_true(all(res$status[!sel] %in% c("extreme", "ok")))
  })
}

test_that("the PCM keeps items with fewer categories", {
  res <- pp_quiet(PPass(erm_fits$PCM, type = "mle"))
  thres <- attr(res, "ppeo")$model$thres
  expect_equal(colSums(!is.na(thres)) - 1, apply(erm_data$p, 2, max), ignore_attr = TRUE)
})

test_that("group contrasts: each group gets the parameters of its virtual items", {
  o <- suppressWarnings(eRm::LPCM(erm_data$p2, mpoints = 2, groupvec = rep(1:2, 125)))
  expect_error(suppressWarnings(eRm::person.parameter(o)), "not possible")
  res <- pp_quiet(PPass(o, type = "mle"))
  expect_identical(res$group, rep(1:2, 125))
  expect_match(attr(res, "info")$model, "2 time points, 2 groups")
  # reference: the parameters of group g by the names in betapar
  nm <- names(o$betapar)
  for (g in 1:2) {
    b <- o$betapar[grepl(paste0(" g", g, "$"), nm)]
    item <- sub("\\.c[0-9]+ t", " t", sub("^beta ", "", names(b)))
    thres <- matrix(NA, 4, 12)
    for (v in seq_along(unique(item))) {
      d <- c(0, -diff(c(0, b[item == unique(item)[v]])))
      thres[seq_along(d), v] <- d
    }
    rows <- o$groupvec == g
    ref <- pp_quiet(PP_gpcm(o$X[rows, ], thres, slopes = rep(1, 12), type = "mle"))
    expect_equal(res$estimate[rows], ref$estimates$estimate, ignore_attr = TRUE)
    expect_equal(res$SE[rows], ref$estimates$SE, ignore_attr = TRUE)
  }
})

test_that("all estimation types and fit indices run on eRm objects", {
  for (o in erm_fits[c("RM", "RSM", "LPCM_2t")]) {
    for (type in c("wle", "map")) {
      res <- pp_quiet(PPass(o, type = type, SE = TRUE))
      expect_true(all(c("lz", "lzstar", "infit", "outfit", "lz_se", "ou_se") %in% names(res)))
      expect_true(all(is.finite(res$lzstar[res$status == "ok"])))
    }
    expect_message(res <- pp_quiet(PPass(o, type = "eap")), "eap estimates only")
    expect_identical(dim(PV(res, npv = 2)$pvdraws), c(nrow(res), 2L))
  }
})

test_that("item parameters cannot be given with an eRm object", {
  expect_error(PPass(erm_fits$RM, thres = 1:8), "leave out 'thres'")
  expect_error(PPass(erm_fits$PCM, slopes = 1, respm = 1), "'respm', 'slopes'|'slopes', 'respm'")
  expect_error(PPass(structure(list(model = "XYZ"), class = "eRm")), "knows the eRm models")
})

test_that("PPass.Rm of 0.6.4 is replaced by PPass.eRm", {
  expect_null(utils::getS3method("PPass", "Rm", optional = TRUE))
  expect_type(utils::getS3method("PPass", "eRm", optional = TRUE), "closure")
})

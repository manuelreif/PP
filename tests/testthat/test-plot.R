# plot() for ppeo and ppass results (R/plot.R). The plots are drawn on a null
# device; the tests check the returned data against the references.

on_null_device <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expr
}

# 2PL set with three reversed response patterns (misfitting persons)
p2 <- items_to_pp(set_2pl$items)
X_mis <- set_2pl$X
X_mis[c(2, 6, 11), ] <- 1L - X_mis[c(2, 6, 11), ]
df_mis <- data.frame(id = sprintf("P%02d", seq_len(nrow(X_mis))), X_mis)
ppass_mis <- function(...) {
  pp_quiet(PPass(df_mis, items = 2:13, thres = p2$b, slopes = p2$slopes, ...))
}

ref_test_info <- function(items, theta) rowSums(sapply(items, ref_item_info, theta = theta))

# Expected score and variance of the response to item at theta
ref_moments <- function(item, theta) {
  p <- ref_catprob(item, theta)
  k <- seq_along(p) - 1
  E <- sum(k * p)
  c(E = E, V = sum((k - E)^2 * p))
}


test_that("test_info() is the sum of the reference item informations", {
  th <- seq(-4, 4, by = 0.25)
  for (nm in names(sets_all)) {
    set <- sets_all[[nm]]
    obj <- pp_fit(set, "wle")
    info <- PP:::test_info(obj$model$bank, th)
    expect_close(info, sapply(set$items, ref_item_info, theta = th), tol = 1e-6)
  }
})

test_that("precision: the standard errors lie on the curve of the test information", {
  for (nm in names(sets_all)) {
    set <- sets_all[[nm]]
    for (type in c("mle", "wle", "map")) {
      obj <- pp_fit(set, type)
      d <- on_null_device(plot(obj, which = "precision"))
      ok <- obj$estimates$status == "ok"
      expect_equal(d$persons$person, which(ok))
      expect_equal(d$persons$SE, obj$estimates$SE[ok])
      prior <- if (type == "map") 1 else 0
      expect_close(d$curve$SE, 1 / sqrt(ref_test_info(set$items, d$curve$theta) + prior), tol = 1e-6)
      # complete data: the SE of each person is the curve at the estimate
      expect_close(d$persons$SE, 1 / sqrt(ref_test_info(set$items, d$persons$estimate) + prior),
                   tol = 1e-6)
    }
  }
})

test_that("precision: the item location is the maximum of the item information", {
  th <- seq(-6, 6, by = 0.001)
  for (nm in names(sets_all)) {
    set <- sets_all[[nm]]
    obj <- pp_fit(set, "wle")
    d <- on_null_device(plot(obj, which = "precision"))
    loc <- vapply(set$items, function(it) th[which.max(ref_item_info(it, th))], 0)
    expect_close(d$items$location, loc, tol = diff(obj$model$range) / 1200 + 0.001)
  }
})

test_that("precision: the curve is drawn only when it holds for every person", {
  # missing responses
  obj <- pp_fit(set_2pl, "wle", X = make_na(set_2pl$X, 7))
  expect_null(on_null_device(plot(obj))$curve)
  d <- on_null_device(plot(obj, curve = TRUE))
  expect_close(d$curve$SE, 1 / sqrt(ref_test_info(set_2pl$items, d$curve$theta)), tol = 1e-6)
  # person-specific priors
  n <- nrow(set_2pl$X)
  obj <- pp_fit(set_2pl, "map", mu = prior_mu(n), sigma2 = prior_s2(n))
  expect_null(on_null_device(plot(obj))$curve)
  # one prior for all persons: 1 / sqrt(I + 1 / sigma2)
  obj <- pp_fit(set_2pl, "eap", sigma2 = 2)
  d <- on_null_device(plot(obj))
  expect_close(d$curve$SE, 1 / sqrt(ref_test_info(set_2pl$items, d$curve$theta) + 0.5), tol = 1e-6)
  expect_null(on_null_device(plot(obj, curve = FALSE))$curve)
})

test_that("fit map: the flagged persons are those of summary()", {
  res <- ppass_mis()
  d <- on_null_device(plot(res, label = "id"))
  s <- pp_quiet(summary(res))
  ok <- res$status == "ok" & is.finite(res$lzstar)
  expect_equal(d$person, which(ok))
  expect_equal(d$value, res$lzstar[ok])
  expect_equal(d$label, df_mis$id[ok])
  expect_setequal(d$person[d$flagged], s$flagged$person)
  expect_true(all(c(2, 6, 11) %in% d$person[d$flagged]))
  # another index and level
  d <- on_null_device(plot(res, index = "ou_t", alpha = 0.01))
  expect_equal(d$value, res$ou_t[is.finite(res$ou_t) & res$status == "ok"])
  expect_equal(d$flagged, abs(d$value) > qnorm(0.995))
})

test_that("fit map of an estimation result computes the fit with Pfit()", {
  obj <- pp_fit(set_2pl, "wle", X = X_mis)
  pf <- pp_quiet(Pfit(pp = obj, fitindices = c("lzstar", "lz")))
  d <- on_null_device(plot(obj, which = "fit"))
  expect_equal(d$value, unname(pf$lzstar[d$person, "lzstar"]))
  d <- on_null_device(plot(obj, which = "fit", index = "lz"))
  expect_equal(d$value, unname(pf$lz[d$person, "lz"]))
})

test_that("profile: expected scores and residuals agree with the reference", {
  for (nm in names(sets_all)) {
    set <- sets_all[[nm]]
    X <- make_na(set$X, 11)
    obj <- pp_fit(set, "wle", X = X)
    person <- utils::head(which(obj$estimates$status == "ok"), 3)
    d <- on_null_device(plot(obj, which = "profile", person = person))
    expect_equal(nrow(d), sum(!is.na(X[person, ])))
    j <- as.integer(sub("^I", "", d$item))
    th <- obj$estimates$estimate[d$person]
    mo <- t(mapply(function(j, t) ref_moments(set$items[[j]], t), j, th))
    expect_equal(d$response, X[cbind(d$person, j)])
    expect_equal(d$max, vapply(set$items[j], ref_ncat, 1L) - 1L)
    expect_close(d$expected, mo[, "E"], tol = 1e-8)
    expect_close(d$z, (d$response - mo[, "E"]) / sqrt(mo[, "V"]), tol = 1e-6)
    expect_equal(d$unexpected, abs(d$z) > qnorm(0.975))
  }
})

test_that("profile: by default the flagged persons with the strongest misfit", {
  res <- ppass_mis()
  d <- on_null_device(plot(res, which = "profile"))
  fl <- which(res$status == "ok" & abs(res$lzstar) > qnorm(0.975))
  expect_equal(unique(d$person), utils::head(fl[order(res$lzstar[fl])], 4))
  # for the outfit t the highest values
  d <- on_null_device(plot(res, which = "profile", index = "ou_t"))
  fl <- which(res$status == "ok" & abs(res$ou_t) > qnorm(0.975))
  expect_equal(unique(d$person), utils::head(fl[order(-res$ou_t[fl])], 4))
})

test_that("plots work for every model and estimator without warnings", {
  for (nm in names(sets_all)) {
    for (type in c("mle", "wle", "map", "eap", "robust")) {
      obj <- pp_fit(sets_all[[nm]], type)
      ok1 <- which(obj$estimates$status == "ok")[1]
      expect_no_warning(on_null_device(plot(obj)))
      expect_no_warning(on_null_device(plot(obj, which = "profile", person = ok1)))
      if (type %in% c("mle", "wle", "map")) {
        expect_no_warning(on_null_device(plot(obj, which = "fit")))
      }
    }
  }
  # PPass without person fit: the default is the precision plot
  res <- suppressMessages(ppass_mis(type = "eap"))
  expect_named(on_null_device(plot(res)), c("persons", "curve", "items"))
  # the device is left with one panel
  on_null_device({
    plot(ppass_mis(), which = "precision")
    expect_equal(par("mfrow"), c(1, 1))
  })
})

test_that("plot() rejects wrong input", {
  res <- ppass_mis()
  obj_mle <- pp_fit(set_2pl, "mle")
  obj_eap <- pp_fit(set_2pl, "eap")
  res_eap <- suppressMessages(ppass_mis(type = "eap"))
  on_null_device({
    expect_error(plot(obj_eap, which = "fit"), "type 'mle', 'wle' or 'map'")
    expect_error(plot(res_eap, which = "fit"), "has no person fit")
    expect_error(plot(res_eap, which = "profile"), "has no person fit")
    expect_error(plot(res[-1, ]), "not a complete result of PPass")
    expect_error(plot(obj_mle, which = "profile", person = 4), "status 'ok'.*4 \\(extreme\\)")
    expect_error(plot(res, which = "profile", person = 0), "'person' must be")
    expect_error(plot(res, which = "profile", person = 1.5), "'person' must be")
    expect_error(plot(res, which = "profile", person = c(2, 2)), "'person' must be")
    expect_error(plot(res, which = "profile", person = 99), "'person' must be")
    expect_error(plot(res, index = "infit"), "'index' must be one of")
    expect_error(plot(res, label = "nope"), "'label' must be the name")
    expect_error(plot(res, curve = TRUE), "'curve' is only used")
    expect_error(plot(res, which = "fit", person = 2), "'person' is only used")
    expect_error(plot(res, which = "precision", curve = NA), "'curve' must be")
    expect_error(plot(res, alpha = 2), "'alpha'")
  })
  # no flagged person: profile needs 'person'
  res_ok <- pp_quiet(PPass(data.frame(set_2pl$X), thres = p2$b, slopes = p2$slopes))
  expect_error(on_null_device(plot(res_ok, which = "profile", alpha = 1e-10)), "No person is flagged")
})

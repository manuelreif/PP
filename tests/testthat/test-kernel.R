# The C++ item kernel (src/items.h) through its internal hooks
# pp_item_eval_cpp() and pp_catprob_cpp(): ll, score, info, J and the category
# probabilities against the plain-R references, also in the far tails.

# Kernel results for every item at every theta, with response x for all items
# (NA where item j has no category x)
kernel_eval <- function(bank, theta, x) {
  X <- matrix(as.integer(x), length(theta), bank$n)
  PP:::pp_item_eval_cpp(bank, theta, X)
}

kernel_items <- c(unlist(lapply(sets_all, `[[`, "items"), recursive = FALSE),
                  set_gpcm12$items,
                  list(item_4pl(2.5, 0.3), item_4pl(1.3, 0.4, 0.25, 0.9),
                       item_gpcm(3, c(-1, 0, 1, 2))))
theta_grid <- seq(-4, 4, by = 0.5)

test_that("the kernel matches the references for all item types", {
  bank <- kernel_bank(kernel_items)
  ncat <- vapply(kernel_items, ref_ncat, 1L)
  for (x in 0:(max(ncat) - 1)) {
    r <- kernel_eval(bank, theta_grid, x)
    for (j in which(ncat > x)) {
      it <- kernel_items[[j]]
      expect_close(r$ll[, j], log(ref_catprob(it, theta_grid)[, x + 1]), tol = 1e-10)
      expect_close(r$score[, j], ref_item_score(it, x, theta_grid), tol = 1e-6)
      expect_close(r$info[, j], ref_item_info(it, theta_grid), tol = 1e-6)
      expect_close(r$J[, j], ref_item_J(it, theta_grid), tol = 1e-5)
    }
    # categories that do not exist give NA
    expect_true(all(is.na(r$ll[, ncat <= x])))
  }
})

test_that("the category probabilities match ref_catprob and sum to 1", {
  bank <- kernel_bank(kernel_items)
  P <- PP:::pp_catprob_cpp(bank, theta_grid)
  ncat <- vapply(kernel_items, ref_ncat, 1L)
  expect_identical(dim(P), c(length(theta_grid), sum(ncat)))
  item_of <- rep(seq_along(ncat), ncat)
  for (j in seq_along(kernel_items)) {
    expect_close(P[, item_of == j], ref_catprob(kernel_items[[j]], theta_grid), tol = 1e-12)
  }
  expect_close(t(rowsum(t(P), item_of)), matrix(1, length(theta_grid), length(ncat)), tol = 1e-12)
})

test_that("the log category probabilities match the kernel ll, also in the far tails", {
  items <- c(kernel_items, list(item_4pl(1, 0), item_4pl(1, 0, 0.2, 0.9)))
  bank <- kernel_bank(items)
  ncat <- vapply(items, ref_ncat, 1L)
  item_of <- rep(seq_along(ncat), ncat)
  cat_of <- sequence(ncat) - 1L
  for (th in list(theta_grid, c(-800, -50, 50, 800))) {
    lp <- PP:::pp_logcatprob_cpp(bank, th)
    expect_identical(dim(lp), c(length(th), sum(ncat)))
    expect_true(all(is.finite(lp)))
    # the same value as the ll of the kernel for each category
    for (x in 0:(max(ncat) - 1)) {
      r <- kernel_eval(bank, th, x)
      cols <- which(cat_of == x)
      expect_identical(lp[, cols], r$ll[, item_of[cols]])
    }
  }
  expect_close(exp(PP:::pp_logcatprob_cpp(bank, theta_grid)), PP:::pp_catprob_cpp(bank, theta_grid),
               tol = 1e-12)
})

test_that("an item gives the same values alone and inside a mixed bank", {
  items <- list(set_gpcm12$items[[1]], item_4pl(1.3, 0.4, 0.25, 0.9),
                item_gpcm(0.8, c(-1, 0.5)), item_4pl(1, -0.7))
  all <- kernel_eval(kernel_bank(items), theta_grid, 1)
  for (j in seq_along(items)) {
    one <- kernel_eval(kernel_bank(items[j]), theta_grid, 1)
    for (q in names(all)) expect_identical(all[[q]][, j], one[[q]][, 1])
  }
})

test_that("the person fit moments match the references for all item types", {
  bank <- kernel_bank(kernel_items)
  ncat <- vapply(kernel_items, ref_ncat, 1L)
  th <- c(theta_grid, NA, Inf)
  for (x in 0:(max(ncat) - 1)) {
    X <- matrix(as.integer(x), length(th), bank$n)
    X[, ncat <= x] <- NA
    X[2, 1] <- NA
    m <- PP:::pp_moments_cpp(bank, X, th)
    expect_named(m, c("E", "V", "C", "ll", "Ell", "Vll", "Cs", "info", "J"))
    for (j in which(ncat > x)) {
      it <- kernel_items[[j]]
      ok <- setdiff(seq_along(theta_grid), if (j == 1) 2)
      p <- ref_catprob(it, theta_grid[ok])
      lp <- log(p)
      dp <- num_d1(function(t) ref_catprob(it, t), theta_grid[ok])
      mom <- t(vapply(theta_grid[ok], function(t) ref_moments(it, t), c(E = 0, V = 0, C = 0)))
      expect_close(m$E[ok, j], mom[, "E"], tol = 1e-12)
      expect_close(m$V[ok, j], mom[, "V"], tol = 1e-12)
      expect_close(m$C[ok, j], mom[, "C"], tol = 1e-12)
      expect_close(m$ll[ok, j], lp[, x + 1], tol = 1e-12)
      expect_close(m$Ell[ok, j], rowSums(p * lp), tol = 1e-12)
      expect_close(m$Vll[ok, j], rowSums(p * lp^2) - rowSums(p * lp)^2, tol = 1e-10)
      expect_close(m$Cs[ok, j], rowSums(dp * lp), tol = 1e-6)
      expect_close(m$info[ok, j], ref_item_info(it, theta_grid[ok]), tol = 1e-6)
      expect_close(m$J[ok, j], ref_item_J(it, theta_grid[ok]), tol = 1e-5)
    }
    # not answered cells and persons without a finite theta are NA
    for (q in names(m)) {
      expect_true(all(is.na(m[[q]][, ncat <= x])))
      expect_true(all(is.na(m[[q]][length(th) - 0:1, ])))
      expect_true(is.na(m[[q]][2, 1]))
    }
  }
})

test_that("the person fit moments stay finite in the far tails", {
  items <- list(item_4pl(1, 0), item_4pl(2, 0, 0.2), item_4pl(1, 0, 0.2, 0.9),
                item_gpcm(1.5, c(-1, 0, 1)))
  bank <- kernel_bank(items)
  th <- c(-800, -50, 50, 800)
  for (x in 0:1) {
    m <- PP:::pp_moments_cpp(bank, matrix(as.integer(x), length(th), 4), th)
    for (q in names(m)) expect_true(all(is.finite(m[[q]])), info = q)
    expect_true(all(m$V >= 0 & m$Vll >= 0))
  }
})

test_that("C1: the 4PL kernel stays finite and exact in the far tails", {
  items <- list(item_4pl(1, 0), item_4pl(2, 0), item_4pl(1, 0, 0.2), item_4pl(1, 0, 0.2, 0.9))
  bank <- kernel_bank(items)
  z <- c(-800, -50, 50, 800)
  for (x in 0:1) {
    r <- kernel_eval(bank, z, x)
    for (q in names(r)) expect_true(all(is.finite(r[[q]])), info = paste(q, "x =", x))
    # |z| = 50: still representable, so the reference applies
    mid <- abs(z) == 50
    for (j in seq_along(items)) {
      expect_close(r$ll[mid, j], log(ref_catprob(items[[j]], z[mid])[, x + 1]), tol = 1e-10)
    }
  }
  P <- PP:::pp_catprob_cpp(bank, z)
  expect_true(all(P >= 0 & P <= 1))
  expect_close(P[, c(1, 3, 5, 7)] + P[, c(2, 4, 6, 8)], matrix(1, 4, 4), tol = 1e-12)

  r0 <- kernel_eval(bank, z, 0)
  r1 <- kernel_eval(bank, z, 1)
  # 1PL: log P(X = 0 | z = 800) = log plogis(-800) = -800, not -Inf or -700
  expect_close(r0$ll[4, 1], -800, tol = 1e-8)
  expect_close(r1$ll[1, 1], -800, tol = 1e-8)
  expect_close(r1$ll[4, 1], 0, tol = 1e-12)
  expect_lt(max(r0$info[c(1, 4), 1:2]), 1e-200)
  # the asymptotes are the limits: log c for x = 1 at z -> -Inf, log(1 - d) for x = 0
  expect_close(r1$ll[1, 3:4], log(c(0.2, 0.2)), tol = 1e-12)
  expect_close(r0$ll[4, 4], log(0.1), tol = 1e-12)
})

test_that("C2: the GPCM kernel stays finite for extreme abilities", {
  items <- list(item_gpcm(3, c(-1, 0, 1, 2)))
  bank <- kernel_bank(items)
  P <- PP:::pp_catprob_cpp(bank, c(-100, 100))
  expect_close(P, rbind(c(1, 0, 0, 0, 0), c(0, 0, 0, 0, 1)), tol = 1e-12)
  for (x in 0:4) {
    r <- kernel_eval(bank, c(-100, 100), x)
    for (q in names(r)) expect_true(all(is.finite(r[[q]])), info = paste(q, "x =", x))
  }
  expect_close(kernel_eval(bank, 100, 4)$ll, 0, tol = 1e-12)
  expect_close(kernel_eval(bank, -100, 0)$ll, 0, tol = 1e-12)
})

test_that("an inconsistent item bank is rejected", {
  bank <- kernel_bank(kernel_items[1:3])
  bad <- bank
  bad$thr <- bad$thr[-1]
  expect_error(PP:::pp_catprob_cpp(bad, 0), "thr")
  bad <- bank
  bad$a <- bad$a[-1]
  expect_error(PP:::pp_catprob_cpp(bad, 0), "length n")
})

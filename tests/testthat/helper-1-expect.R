# Test infrastructure shared by all test files.
#
# Tests for known bugs (IDs from problems.md) call skip_known() first. They
# describe the correct behaviour and are skipped until the block of the
# refactoring that fixes the bug. Their names start with the problem ID, so the
# list of skips in a test run is the list of open problems.
#
# With the environment variable PP_KNOWN_BUGS=run they are not skipped. On the
# old code every one of them must then fail; this shows that each test really
# detects its bug.

skip_known <- function(id, block) {
  if (!identical(Sys.getenv("PP_KNOWN_BUGS"), "run")) {
    testthat::skip(paste0(id, " - fixed in block ", block))
  }
  invisible(TRUE)
}

# Absolute comparison of numeric vectors or matrices. Missing values (NA/NaN)
# and infinite values must sit at the same positions; the finite values may
# differ by at most tol. expect_equal() compares relative differences, which is
# misleading for abilities close to 0.
expect_close <- function(actual, expected, tol = 1e-3) {
  a <- as.numeric(actual)
  e <- as.numeric(expected)
  if (length(a) != length(e)) {
    testthat::expect(FALSE, sprintf("lengths differ: %d (actual) vs %d (expected)",
                                    length(a), length(e)))
    return(invisible(actual))
  }
  kind <- function(x) ifelse(is.na(x), "NA", ifelse(is.infinite(x), sign(x) * Inf, "finite"))
  ka <- kind(a)
  ke <- kind(e)
  if (any(ka != ke)) {
    i <- which(ka != ke)[1]
    testthat::expect(FALSE, sprintf("element %d: actual %s, expected %s (%d mismatches of NA/Inf)",
                                    i, format(a[i]), format(e[i]), sum(ka != ke)))
    return(invisible(actual))
  }
  fin <- ka == "finite"
  d <- if (any(fin)) max(abs(a[fin] - e[fin])) else 0
  i <- if (any(fin)) which(fin)[which.max(abs(a[fin] - e[fin]))] else NA
  testthat::expect(d <= tol,
                   sprintf("max abs difference %.3g > %.3g (element %d: actual %.6g, expected %.6g)",
                           d, tol, i, a[i], e[i]))
  invisible(actual)
}

# Run a PP call without its cat() output and without the warnings PP gives in
# the documented standard case (default prior, robust GPCM). All other warnings
# stay visible.
pp_noise <- c("all mu's are set to 0", "all sigma2's are set to 1",
              "Robust estimation for GPCM is still very experimental")

pp_quiet <- function(expr) {
  out <- NULL
  utils::capture.output(
    out <- withCallingHandlers(expr, warning = function(w) {
      if (any(startsWith(conditionMessage(w), pp_noise))) invokeRestart("muffleWarning")
    })
  )
  out
}

# Estimates and SEs (n x 2 matrix) of a PP result. The only place to change
# when the result object gets $estimates (block 4).
est <- function(obj) obj$resPP$resPP

# PP 0.7.0

PP 0.7.0 is a rewrite of the numerical core. Every estimate is now checked
against independent reference implementations, and several results of
earlier versions were wrong. Results therefore differ from 0.6.4-1 where
noted below. Function and argument names are unchanged unless stated.

## Corrected results

* **Jackknife (`JKpp()`)**: the standard error was too small by a factor of
  about L − 1 (L = number of items). The jackknife now runs over the answered
  items only (before: all items), leave-one-out samples without a finite
  estimate are counted in the new column `excluded` with a warning instead of
  being dropped silently, and with `killdupli = TRUE` the results were
  assigned to the wrong persons. The EAP jackknife of mixed tests failed.
* **AMT jackknife** (`cmeth = "AMT"`): now the AMT-robustified jackknife of
  Wainer & Wright (1980): Andrews' sine estimator (c = 2.1, MAD scale),
  applied to the pseudovalues of the right and of the wrong answers
  separately and averaged with the group sizes as weights. The old code had a
  sign error, no scale and maximised the sum of the sines.
* **MAP standard errors** now include the prior.
* **Robust estimates**: the standard error is the sandwich form with the
  Huber weights (before: the weights were ignored), and the estimate solves
  its weighted estimating equation (before up to 3e-3 off).
* **WLE/MLE for the 3PL** could oscillate without converging and without a
  warning; the estimator is now a safeguarded Newton method per person.
* **Plausible values (`PV()`)** are drawn from the posterior on a grid
  (`approx = FALSE`) or from a normal distribution with its mean and SD
  (`approx = TRUE`, default). The old Metropolis-Hastings sampler was invalid,
  ignored the prior, failed with missing values and could mix up rows; the
  arguments `thinning`, `burnin` and `mult` are ignored with a warning.
* **EAP estimates** are computed on an adaptive grid with log-likelihoods
  (before: products, which underflowed for long tests and gave NaN).
* **Person fit (`Pfit()`)**: lz\* was wrong with missing responses; infit and
  outfit of the PCM took the number of categories from the data instead of
  the item parameters and ignored the slopes of the GPCM; the jackknife SE of
  lz ignored person-specific priors. Results are no longer rounded (p values
  of 0.0004 were 0).
* Scalar `mu`, `sigma2` or item parameters led to out-of-bounds memory reads
  in C++; they are now recycled.
* Duplicated response patterns with `killdupli = TRUE` could be merged
  wrongly (ambiguous key).
* Numerically stable item probabilities (no overflow in the 4PL, log-sum-exp
  in the GPCM), also in `sim_4pl()` and `sim_gpcm()`.

## Breaking changes

* `PV()` needs an estimate with a prior, i.e. `type = "eap"` or `"map"`; it
  stops for MLE, WLE and robust estimates. These have no prior any more:
  `mu` and `sigma2` given there are ignored with a warning and stored as `NA`.
* Persons whose estimating equation has no root inside `range` (new argument,
  default `c(-6, 6)`) get the bound with status `"boundary"` and a warning
  (before: NaN or drifting values). Persons without any response get `NA`
  (before: `+Inf`); for EAP the prior mean and SD.
* The results are numbered by person in the order of the response matrix;
  row names of `respm` are not carried over (duplicated names made the
  development version fail).
* `PPass()` on eRm objects: the method `PPass.Rm` is replaced by
  `PPass.eRm`, and the result of `PPass()` is a data.frame of class `ppass`
  with the new column `status`. For `type = "eap"` and `"robust"` it contains
  the estimates only (with a message).
* `sim_4pl()` and `sim_gpcm()` draw differently, so simulated data are not
  identical to 0.6.4-1 for the same seed.
* Removed: `lzstar_mixed()`, `Pfit_lzstarmix()` and
  `print_personfit_summary()` (new in 0.6.4-1). `Pfit()` now gives lz and lz\*
  for all models (lz\* after Sinharay, 2016, with the correction of the
  estimation type), `summary()` of its result replaces the printed summary.
  `Pfit_lzstarmix()` used random thresholds when it could not find the item
  parameters.
* `JKpp(maxsteps, exac)` default to the values of the estimation object
  (before 500 and 0.001).

## New features

* Every result has `$estimates`, a data.frame with `estimate`, `SE`,
  `nsteps` and `status` (`"ok"`, `"extreme"`, `"boundary"`, `"no_response"`,
  `"not_converged"`) per person; `coef()` and `as.data.frame()` methods.
  `JKpp()` returns `$estimates` with `L`, `excluded` and `status`.
* New arguments `range` and `verbose` in `PP_4pl()`, `PP_gpcm()` and
  `PPall()`. Defaults are unified: `maxsteps = 100`, `exac = 0.001`.
* `Pfit()` works for all models (1PL–4PL, PCM, GPCM and mixed tests), for
  MLE, WLE and MAP, with missing responses; `respm` is optional (the
  responses come from the estimation object). Jackknife SEs for all four
  statistics (new columns `in_se`, `ou_se`), `print()` and `summary()`.
* `PPass()`:
  * no longer needs `mod`: the model (1PL to 4PL, PCM, GPCM or a mixed test)
    follows from the item parameters, as in `PP_4pl()`; a call with `mod` gives
    a warning and `mod` is ignored. Before, `mod` only switched between
    `PP_4pl()`, `PP_gpcm()` and `PPall()`, and `mod = "MIXED"` did not work.
  * works with a matrix; `items` can be given by name; the other columns
    (e.g. an id) are kept as person variables.
  * takes every eRm model: RM, LLTM, RSM, PCM, LRSM and LPCM, also with
    several time points and groups, including group contrasts, for which
    eRm gives no person parameters. Its maximum likelihood estimates equal
    `eRm::person.parameter()`. eRm is not needed at run time.
  * `print()` and `summary()` give an overview with the persons flagged by
    the person fit; `JKpp()` and `PV()` work directly on the result.
* `plot()` for the results of `PPass()`, `PP_4pl()`, `PP_gpcm()` and
  `PPall()` (help page `?"pp-plots"`), drawn with the new dependency
  tinyplot:
  * `which = "fit"`: person fit map, the fit statistic against the estimate
    with the flagged persons labelled;
  * `which = "precision"`: the standard error of each person and of the
    whole test, 1/sqrt(I(theta)) from the known item parameters, below the
    distribution of the estimates and with the location of every item;
  * `which = "profile"`: observed against expected scores of the answered
    items of single persons, with the unexpected answers labelled.
* `sim_mixed()` simulates tests of 4PL and GPCM items.
* `PPall()`: `slopes`, `lowerA`, `upperA` and `model2est` are optional
  (`model2est` defaults to `findmodel(thres)`); `PP_4pl()` with `lowerA` but
  without `slopes` fits the 3PL with slopes 1.
* Input checks with clear messages for all arguments.
* No startup message, no `cat()` output and no warning for the default
  prior.

## Deprecated

* `$resPP` (estimation objects) and `$resjk` (jackknife) are kept as aliases
  of the 0.6.4 layout; use `$estimates`.

## Performance

The C++ core estimates every person separately with its own convergence
check (before: all persons iterated until the slowest converged). Median
times on 5000 persons × 40 items: 1PL–3PL estimates 0.02 s (before
0.3–2.9 s), EAP 0.03 s (0.8 s); GPCM with 2000 × 20: 0.01 s (0.2–1.3 s);
`JKpp()` on 500 × 40: 0.05 s (142 s); `PV()` 150 times and `sim_gpcm()`
400 times faster.

## Documentation

* New vignettes: "Getting started with PP", "Person fit with PP" and
  "Person assessment with PPass, also for eRm models".
* Examples are part of the help pages.

## Maintenance

* Manuel Reif is the maintainer again. The repository moved to
  <https://github.com/manuelreif/PP>.

# PP 0.6.4-1

* Person fit for mixed tests: `lzstar_mixed()`, `Pfit_lzstarmix()`,
  `print_personfit_summary()`.
* Jan Steinfeld became the maintainer.

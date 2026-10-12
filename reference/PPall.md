# Estimate Person Parameters

Compute person parameters for the 1,2,3,4-PL model and for the GPCM.
Choose between ML, WL, MAP, EAP and robust estimation. Use this function
if 4-PL items and GPCM items are mixed for each person.

## Usage

``` r
PPall(
  respm,
  thres,
  slopes = NULL,
  lowerA = NULL,
  upperA = NULL,
  theta_start = NULL,
  mu = NULL,
  sigma2 = NULL,
  type = "wle",
  model2est = NULL,
  maxsteps = 100,
  exac = 0.001,
  H = 1,
  ctrl = list(),
  range = c(-6, 6),
  verbose = FALSE
)
```

## Arguments

- respm:

  An integer matrix (or a data.frame), which contains the responses of
  the examinees. A persons x items matrix is expected. Missing responses
  are `NA`.

- thres:

  A numeric matrix which contains the threshold parameter for each item.
  If the first row of the matrix is not set to zero (only zeroes in the
  first row) - then a row-vector with zeroes is added by default.

- slopes:

  A numeric vector, which contains the slope parameters for each item -
  one parameter per item is expected. A single value is used for all
  items. If `NULL` (default), all slopes are 1.

- lowerA:

  A numeric vector, which contains the lower asymptote parameters (kind
  of guessing parameter) for each item. It applies to the 4PL items
  only; a value other than 0 for a GPCM item is ignored with a warning.
  A single value is used for all items. If `NULL` (default), all lower
  asymptotes are 0.

- upperA:

  numeric vector, which contains the upper asymptote parameters for each
  item. It applies to the 4PL items only; a value other than 1 for a
  GPCM item is ignored with a warning. A single value is used for all
  items. If `NULL` (default), all upper asymptotes are 1.

- theta_start:

  A vector which contains a starting value for each person. If NULL is
  submitted, the starting values are set automatically. If a scalar is
  submitted, this start value is used for each person.

- mu:

  A numeric vector of location parameters for each person, the normal
  prior for MAP and EAP estimation and for
  [`PV`](https://manuelreif.github.io/PP/reference/PV.md). A single
  value is used for all persons. If nothing is submitted this is set to
  0 for each person. MLE, WLE and robust estimation have no prior: a
  value given there is ignored with a warning, and the result stores
  `NA`.

- sigma2:

  A numeric vector of variance parameters for each person, the normal
  prior for MAP and EAP estimation and for
  [`PV`](https://manuelreif.github.io/PP/reference/PV.md). A single
  value is used for all persons. If nothing is submitted this is set to
  1 for each person. MLE, WLE and robust estimation have no prior: a
  value given there is ignored with a warning, and the result stores
  `NA`.

- type:

  Which maximization should be applied? There are five valid entries
  possible: "mle", "wle", "map", "eap" and "robust". To choose between
  the methods, or just to get a deeper understanding the papers
  mentioned below are quite helpful. The default is `"wle"` which is a
  good choice in many cases. With `"eap"` the estimate is the mean and
  the SE the standard deviation of the posterior, computed on a grid of
  ability values (see
  [PV](https://manuelreif.github.io/PP/reference/PV.md)). Plausible
  values ([`PV`](https://manuelreif.github.io/PP/reference/PV.md)) need
  the prior of `"eap"` or `"map"`.

- model2est:

  A character vector with length equal to the number of submitted items.
  It defines itemwise the response model under which the item parameter
  was estimated. There are 2 valid inputs up to now: `"GPCM"` and
  `"4PL"`. A `"4PL"` item must have exactly two categories in `thres`.
  If `NULL` (default),
  [`findmodel`](https://manuelreif.github.io/PP/reference/findmodel.md)`(thres)`
  is used.

- maxsteps:

  The maximum number of Newton steps per person. Default = 100.

- exac:

  How accurate are the estimates supposed to be? The iteration of a
  person stops when the step is smaller than `exac`. Default is 0.001.

- H:

  In case `type = "robust"` a Huber ability estimate is performed, and
  `H` modulates how fast the downweighting takes place (for more Details
  read Schuster & Yuan 2011).

- ctrl:

  More controls:

  - `killdupli`: Should duplicated response pattern be removed for
    estimation (estimation is faster)? This is especially reasonable in
    case of a large number of examinees and a small number of items. Use
    this option with caution (for map and eap), because persons with
    different `mu` and `sigma2` will have different ability estimates
    despite they responded identically. Default value is `FALSE`.

  - `skipcheck`: Default = FALSE. If TRUE data matrix and arguments are
    not checked - this saves time e.g. when you use this function for
    simulations.

- range:

  The interval `c(lower, upper)` in which mle, wle, map and robust
  estimates are searched. If the estimating equation has no root inside,
  the estimate is set to the bound it points to and its status is
  `"boundary"`. Default `c(-6, 6)`.

- verbose:

  If `TRUE`, a message names the model and the estimation type. Default
  `FALSE`.

## Value

An object of class `ppeo` (and `fourpl`, `gpcm` or `gpcm4pl`), a list
with

- `estimates`:

  A data.frame with one row per person, in the order of `respm`:
  `estimate`, `SE`, `nsteps` (Newton steps of the person; 0 for eap) and
  `status`, a factor with the levels

  - `"ok"`: converged inside `range`,

  - `"extreme"`: mle or robust estimate of a person with all answers in
    the lowest or highest category, the estimate is `-Inf` or `Inf` and
    the SE `NA`,

  - `"boundary"`: the estimating equation has no root inside `range`,
    the estimate is the bound,

  - `"no_response"`: the person answered no item, estimate and SE are
    `NA` (eap: prior mean and SD),

  - `"not_converged"`: no convergence within `maxsteps`.

  A warning reports persons with status `"boundary"` or
  `"not_converged"`.

- `type`, `call`:

  The estimation type and the call.

- `model`:

  The checked and prepared inputs.

- `resPP`:

  The result in the layout of earlier versions: `resPP$resPP` is the
  persons x 2 matrix of estimates and SEs, `resPP$nsteps` the largest
  number of steps.

Use [`coef()`](https://rdrr.io/r/stats/coef.html) for the estimates and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for the
table.

## Details

For a test with both: dichotomous and polytomous items which have been
scaled under 1/2/3/4-PL model or the GPCM, use this function to estimate
the person ability parameters. You have to define the appropriate model
for each item.

Please note, that `robust` estimation with (Huber ability estimate)
polytomous items is still experimental!

## References

Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory -
Parameter Estimation Techniques. CRC-Press.

Barton, M. A., & Lord, F. M. (1981). An Upper Asymptote for the
Three-Parameter Logistic Item-Response Model.

Magis, D. (2013). A note on the item information function of the
four-parameter logistic model. Applied Psychological Measurement, 37(4),
304-315.

Muraki, Eiji (1992). A Generalized Partial Credit Model: Application of
an EM Algorithm. Applied Psychological Measurement, 16, 159-176.

Muraki, Eiji (1993). Information Functions of the Generalized Partial
Credit Model. Applied Psychological Measurement, 17, 351-363.

Samejima, Fumiko (1993). The bias function of the maximum likelihood
estimate of ability for the dichotomous response level. Psychometrika,
58, 195-209.

Samejima, Fumiko (1993). An approximation of the bias function of the
maximum likelihood estimate of a latent variable for the general case
where the item responses are discrete. Psychometrika, 58, 119-138.

Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability
in item response models. Journal of Educational and Behavioral
Statistics, 36(6), 720-735.

Wang, S. and Wang, T. (2001). Precision of Warm's Weighted Likelihood
Estimates for a Polytomous Model in Computerized Adaptive Testing.
Applied Psychological Measurement, 25, 317-331.

Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In
Item Response Theory. Psychometrika, 54, 427-450.

Yen, Y.-C., Ho, R.-G., Liao, W.-W., Chen, L.-J., & Kuo, C.-C. (2012). An
empirical evaluation of the slip correction in the four parameter
logistic models with computerized adaptive testing. Applied
Psychological Measurement, 36, 75-87.

## See also

[PPass](https://manuelreif.github.io/PP/reference/PPass.md),
[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[JKpp](https://manuelreif.github.io/PP/reference/Jkpp.md),
[PV](https://manuelreif.github.io/PP/reference/PV.md)

## Author

Manuel Reif

## Examples

``` r
# items 1 to 3 are 4PL items (one threshold), items 4 to 6 GPCM items
thres <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
thres[2, 1:3] <- NA
findmodel(thres)
#> [1] "4PL"  "4PL"  "4PL"  "GPCM" "GPCM" "GPCM"
sl <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
la <- c(0.02, 0.1, 0, 0, 0, 0)
ua <- c(0.97, 0.91, 1, 1, 1, 1)
set.seed(1811)
x <- sim_mixed(thres = thres, alpha = sl, lowerA = la, upperA = ua, theta = rnorm(30))

# model2est defaults to findmodel(thres)
res <- PPall(respm = x, thres = thres, slopes = sl, lowerA = la, upperA = ua, type = "wle")
summary(res)
#> PP Version: 0.7.0 
#> 
#> Call: PPall(respm = x, thres = thres, slopes = sl, lowerA = la, upperA = ua,      type = "wle") 
#> - job started @ Mon Oct 12 12:14:34 2026 
#> 
#> Estimation type: wle 
#> 
#> Status of the persons:
#> 
#>            ok       extreme      boundary   no_response not_converged 
#>            30             0             0             0             0 
#> 
#> Newton steps: at most 5 
#> -------------------------------------
#>    estimate     SE nsteps status
#> 1   -0.5295 0.7814      4     ok
#> 2   -1.2142 0.9117      3     ok
#> 3   -0.5404 0.7828      4     ok
#> 4   -0.1799 0.7431      2     ok
#> 5   -1.2071 0.9098      4     ok
#> 6   -0.8046 0.8232      5     ok
#> 7    1.1510 0.7236      5     ok
#> 8   -0.5404 0.7828      4     ok
#> 9   -1.1249 0.8893      3     ok
#> 10  -0.3277 0.7576      4     ok
#> 11   0.8426 0.7044      4     ok
#> 12   1.8035 0.8375      4     ok
#> 13  -0.4755 0.7745      4     ok
#> 14   0.6429 0.7016      4     ok
#> 15  -0.5404 0.7828      4     ok
#> --------> output truncated <--------

res_eap <- PPall(respm = x, thres = thres, slopes = sl, lowerA = la, upperA = ua, type = "eap")
head(res_eap$estimates)
#>     estimate        SE nsteps status
#> 1 -0.4288995 0.6124392      0     ok
#> 2 -0.8432752 0.6732341      0     ok
#> 3 -0.4364065 0.6128774      0     ok
#> 4 -0.1935173 0.6205010      0     ok
#> 5 -0.8688358 0.6430377      0     ok
#> 6 -0.6142257 0.6254959      0     ok
```

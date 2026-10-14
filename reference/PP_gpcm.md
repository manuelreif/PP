# Estimate Person Parameters for the GPCM

Compute person parameters for the GPCM and choose between five common
estimation techniques: ML, WL, MAP, EAP and a robust estimation. All
item parameters are treated as fixed.

## Usage

``` r
PP_gpcm(
  respm,
  thres,
  slopes = NULL,
  theta_start = NULL,
  mu = NULL,
  sigma2 = NULL,
  type = "wle",
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

  No longer used. Its options `killdupli` and `skipcheck` of version
  0.6.4 saved time with the old estimation routine; since 1.0.0 every
  person is estimated separately, and options given are ignored with a
  warning.

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

Use [`coef()`](https://rdrr.io/r/stats/coef.html) for the estimates,
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for the
table and [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
the measurement precision, the person fit map or person profiles (see
[pp-plots](https://manuelreif.github.io/PP/reference/pp-plots.md)).

## Details

Please note, that `robust` estimation with (Huber ability estimate)
polytomous items is still experimental!

The probability choosing the k-th category is as follows:

\$\$P(x\_{ij} = k \| \hat \alpha_i, \hat\beta\_{iv}, \theta_j) =
\frac{exp(\sum\_{v=0}^{(k-1)}\hat \alpha\_{i}(\theta_j - \hat
\beta\_{iv}))}{\\\sum\_{c=0}^{m_i - 1}exp(\sum\_{v=0}^{c}\hat
\alpha\_{i}(\theta_j - \hat \beta\_{iv})))}\$\$

In our case \\\theta\\ is to be estimated. The item parameters are
assumed as fixed (usually these are estimates of a former scaling
procedure).

The model simplifies to the Partial Credit Model by setting
\\\alpha\_{i} = 1, \forall i\\.

## References

Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory -
Parameter Estimation Techniques. CRC-Press.

Masters, G. N. (1982). A Rasch model for partial credit scoring.
Psychometrika, 47(2), 149-174.

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

## See also

[PPass](https://manuelreif.github.io/PP/reference/PPass.md),
[PPall](https://manuelreif.github.io/PP/reference/PPall.md),
[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[JKpp](https://manuelreif.github.io/PP/reference/Jkpp.md),
[PV](https://manuelreif.github.io/PP/reference/PV.md)

## Author

Manuel Reif

## Examples

``` r
# six items with three categories: one column of thresholds per item
thres <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
sl <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
set.seed(1750)
x <- sim_gpcm(thres = thres, alpha = sl, theta = rnorm(30))

# GPCM, weighted likelihood
resgpcm <- PP_gpcm(respm = x, thres = thres, slopes = sl, type = "wle")
summary(resgpcm)
#> PP Version: 1.0.0 
#> 
#> Call: PP_gpcm(respm = x, thres = thres, slopes = sl, type = "wle") 
#> - job started @ Wed Oct 14 13:06:25 2026 
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
#> 1  -1.71563 0.9957      4     ok
#> 2   1.33733 0.6221      5     ok
#> 3   1.06287 0.6205      4     ok
#> 4   1.40762 0.6244      5     ok
#> 5   2.18275 0.7115      3     ok
#> 6   1.77794 0.6514      5     ok
#> 7   0.69604 0.6338      4     ok
#> 8   0.29161 0.6615      3     ok
#> 9   0.07075 0.6798      3     ok
#> 10 -0.65730 0.7579      4     ok
#> 11  1.06287 0.6205      4     ok
#> 12  1.02014 0.6212      4     ok
#> 13 -0.02115 0.6879      3     ok
#> 14  0.07075 0.6798      3     ok
#> 15  1.23916 0.6202      5     ok
#> --------> output truncated <--------

# partial credit model: all slopes 1 (the default), EAP
respcm <- PP_gpcm(respm = x, thres = thres, type = "eap")
head(respcm$estimates)
#>     estimate        SE nsteps status
#> 1 -1.4398282 0.6313135      0     ok
#> 2  0.6271971 0.5680080      0     ok
#> 3  0.6271971 0.5680080      0     ok
#> 4  0.9511157 0.5710108      0     ok
#> 5  1.6241091 0.5931807      0     ok
#> 6  1.2812317 0.5790233      0     ok

# items with a different number of categories: NA below the last threshold
thres4 <- rbind(thres, c(NA, NA, NA, NA, 1.7, 1))
x4 <- sim_gpcm(thres = thres4, alpha = sl, theta = rnorm(30))
head(PP_gpcm(respm = x4, thres = thres4, slopes = sl, type = "map")$estimates)
#>     estimate        SE nsteps status
#> 1  0.1099944 0.5332622      3     ok
#> 2 -0.3784739 0.5724116      3     ok
#> 3 -0.7512036 0.6033999      3     ok
#> 4 -0.7584931 0.6040396      4     ok
#> 5 -1.0960477 0.6357668      4     ok
#> 6 -1.3485548 0.6623468      4     ok
```

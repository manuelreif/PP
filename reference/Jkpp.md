# Run a jackknife

This function uses a jackknife approach to compute person parameters.
The jackknife ability measure is based on primarily estimated models
([`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md) or
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md)) - so
the function is applied on the estimation objects, and jackknifed
ability measures are returned.

## Usage

``` r
JKpp(estobj, ...)

# S3 method for class 'ppeo'
JKpp(
  estobj,
  cmeth = "mean",
  maxsteps = NULL,
  exac = NULL,
  fullmat = FALSE,
  ...
)

# S3 method for class 'jk'
print(x, digits = 4, ...)

# S3 method for class 'jk'
summary(object, nrowmax = 15, digits = 4, ...)

# S3 method for class 'ppass'
JKpp(estobj, ...)
```

## Arguments

- estobj:

  An object which originates from using
  [`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
  [`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md) or
  [`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md).

- ...:

  More input.

- cmeth:

  Choose the centering method, to summarize the pseudovalues of a person
  to one single ability estimate. There are three valid entries: "mean",
  "median" and "AMT" (see Details for further description).

- maxsteps:

  The maximum number of Newton steps per leave-one-out sample. If `NULL`
  (default), the value of `estobj` is used.

- exac:

  How accurate are the leave-one-out estimates supposed to be? If `NULL`
  (default), the value of `estobj` is used.

- fullmat:

  Default = FALSE. If TRUE, the function returns the whole jackknife
  matrix of leave-one-out estimates, which is the basis for the
  jackknife estimator.

- x, object:

  An object of class `jk`, the result of `JKpp()`.

- digits:

  Number of significant digits to print.

- nrowmax:

  [`summary()`](https://rdrr.io/r/base/summary.html): how many persons
  to show. Default = 15.

## Value

An object of class `jk`, a list with

- `estimates`:

  A data.frame with one row per person (in the order of `respm`):
  `estimate` and `SE` of the jackknife, `L` the number of answered
  items, `excluded` the number of leave-one-out samples without a finite
  estimate and `status` (`"ok"`, `"too_few_items"` or the status of the
  full estimate).

- `resjk`:

  The matrix of `estimate` and `SE`, as in earlier versions.

- `type`, `cmeth`, `call`:

  The estimation type of `estobj`, the centering method and the call.

- `jk_mat`:

  Only with `fullmat = TRUE`: the persons x items matrix of
  leave-one-out estimates, `NA` for items not answered and excluded
  samples.

## Details

For each person, every answered item is left out once and the ability is
estimated again from the remaining items, with the same estimation type,
prior and `range` as in `estobj` (MLE, WLE, MAP and robust estimates
start at the full estimate; EAP estimates use the posterior grid of the
full estimate). Items a person did not answer are not left out: with
\\L\\ answered items and the leave-one-out estimates
\\\hat\theta\_{(-j)}\\, the pseudovalues are \\L\hat\theta -
(L-1)\hat\theta\_{(-j)}\\. The jackknife estimate is their mean, median
or AMT (`cmeth`), the standard error is \\\sqrt{\sum_j (ps_j -
\bar{ps})^2 / (L(L-1))}\\ (Wainer & Wright, 1980).

A leave-one-out sample may have no finite estimate, e.g. with
`type = "mle"` when the only wrong answer is left out. Such samples (and
samples at the bound of `range` or not converged) are left out of the
person's jackknife, counted in the column `excluded`, and a warning is
given; WLE avoids this in most cases. A person whose full estimate has
no solution (status `"extreme"`, `"no_response"`, `"boundary"` or
`"not_converged"` in `estobj$estimates`) or who has fewer than two
usable leave-one-out samples (status `"too_few_items"`) gets `NA`.

Up to version 0.6.4 the standard error mixed the leave-one-out estimates
with the formula for the pseudovalues and was too small by a factor of
about \\L-1\\, all items counted as answered, and leave-one-out samples
without a finite estimate were dropped silently.

**AMT-robustified jackknife:** When choosing `cmeth = "AMT"`, the
pseudovalues are combined as in scheme 4 of Wainer & Wright (1980): they
are split into the pseudovalues of the right and of the wrong answers
(polytomous items: answers at or above the expected score at
\\\hat\theta\\ and below it), each group is summarised by Andrews' sine
M-estimator (AMT), one of the winners in the Princeton Robustness Study
(Andrews et al., 1972), and the two values are averaged with the group
sizes as weights. Within a group, \\T\\ solves \\\sum_j \psi((ps_j -
T)/s) = 0\\ with \\\psi(x) = \sin(x/2.1)\\ for \\\|x\| \le 2.1\pi\\ and
0 otherwise, where \\s\\ is the median absolute deviation of the group's
pseudovalues from their median; pseudovalues further than \\2.1 \pi s\\
from \\T\\ get no weight. Up to version 0.6.4 the AMT had a sign error,
no scale and maximised the sum of the sines.

## References

Wainer, H., & Wright, B. D. (1980). Robust estimation of ability in the
Rasch model. Psychometrika, 45(3), 373-391.

Andrews, D. F., Bickel, P. J., Hampel, F. R., Huber, P. J., Rogers, W.
H., & Tukey, J. W. (1972). Robust estimates of location: Survey and
advances. Princeton University Press.

## See also

[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[PPall](https://manuelreif.github.io/PP/reference/PPall.md)

## Author

Manuel Reif

## Examples

``` r
set.seed(1623)
b  <- seq(-3, 3, length = 12)
sl <- round(runif(12, 0.5, 1.5), 2)
x  <- sim_4pl(beta = b, alpha = sl, lowerA = 0, upperA = 1, theta = rnorm(20, sd = 1.5))

res <- PP_4pl(respm = x, thres = b, slopes = sl, type = "wle")
jk <- JKpp(res)
summary(jk)
#> PP Version: 1.0.0 
#> 
#> Call: JKpp.ppeo(estobj = res) 
#> - job started @ Wed Oct 14 13:06:24 2026 
#> 
#> Jackknife of the wle estimates, centering method: mean 
#> 
#> Status of the persons:
#> 
#>            ok       extreme      boundary   no_response not_converged 
#>            20             0             0             0             0 
#> too_few_items 
#>             0 
#> -------------------------------------
#>    estimate     SE  L excluded status
#> 1   0.60664 0.7999 12        0     ok
#> 2   0.98253 1.1878 12        0     ok
#> 3  -1.40716 0.8673 12        0     ok
#> 4  -1.19285 0.8581 12        0     ok
#> 5   2.05762 0.7264 12        0     ok
#> 6   0.80729 1.0734 12        0     ok
#> 7   0.23162 1.3887 12        0     ok
#> 8  -1.20003 0.9877 12        0     ok
#> 9   0.04719 0.8299 12        0     ok
#> 10 -1.33332 0.9317 12        0     ok
#> 11  1.72582 1.2356 12        0     ok
#> 12 -1.75695 0.7775 12        0     ok
#> 13  0.97960 1.2878 12        0     ok
#> 14  0.35344 0.7533 12        0     ok
#> 15  0.67128 0.9412 12        0     ok
#> --------> output truncated <--------
# WLE and jackknife side by side
head(cbind(res$estimates[, c("estimate", "SE")], jk = jk$estimates[, c("estimate", "SE")]))
#>     estimate        SE jk.estimate     jk.SE
#> 1  0.5814346 0.8916932   0.6066445 0.7998608
#> 2  1.0410633 0.9250874   0.9825260 1.1877911
#> 3 -1.2735403 0.7988159  -1.4071573 0.8673315
#> 4 -1.0334031 0.7971034  -1.1928456 0.8581273
#> 5  1.9535007 1.0118405   2.0576170 0.7263699
#> 6  0.9155812 0.9157085   0.8072867 1.0733780

# robust centering: the AMT of Wainer & Wright (1980)
head(JKpp(res, cmeth = "AMT")$estimates)
#>     estimate        SE  L excluded status
#> 1  0.5652167 0.7998608 12        0     ok
#> 2  0.9203781 1.1877911 12        0     ok
#> 3 -0.8327968 0.8673315 12        0     ok
#> 4 -0.4534002 0.8581273 12        0     ok
#> 5  1.6376649 0.7263699 12        0     ok
#> 6  0.2013954 1.0733780 12        0     ok

# partial credit model, EAP
thres <- rbind(0, c(-1, -0.5, 0, 0.4), c(0.5, 0.8, 1, 1.5))
xg <- sim_gpcm(thres = thres, alpha = 1, theta = rnorm(20))
JKpp(PP_gpcm(respm = xg, thres = thres, type = "eap"))
#>    estimate     SE L excluded status
#> 1    0.3121 0.5914 4        0     ok
#> 2   -0.6551 0.2788 4        0     ok
#> 3   -0.6565 0.3595 4        0     ok
#> 4    2.3861 0.1220 4        0     ok
#> 5    1.2258 0.2669 4        0     ok
#> 6   -1.2304 0.2838 4        0     ok
#> 7    0.3067 0.3844 4        0     ok
#> 8   -1.2304 0.2838 4        0     ok
#> 9   -0.6170 0.6238 4        0     ok
#> 10   0.3029 0.1472 4        0     ok
#> 11   1.2355 0.4605 4        0     ok
#> 12   1.2284 0.4180 4        0     ok
#> 13  -0.1389 0.5493 4        0     ok
#> 14  -0.1573 0.2297 4        0     ok
#> 15   1.2391 0.3869 4        0     ok
#> 16  -0.1371 0.5600 4        0     ok
#> 17  -0.6659 0.4163 4        0     ok
#> 18   1.2258 0.2669 4        0     ok
#> 19  -1.2455 0.4852 4        0     ok
#> 20  -0.6551 0.2788 4        0     ok
```

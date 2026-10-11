# Person-Fit statistics

Compute the person fit statistics lz, lz\*, infit and outfit for the
1-PL, 2-PL, 3-PL, 4-PL model, the GPCM and mixed tests, at the person
estimates of
[`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md) or
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md).

## Usage

``` r
Pfit(respm, pp, fitindices, SE = FALSE)

# S3 method for class 'ppeo'
Pfit(respm, pp, fitindices = c("lz", "lzstar", "infit", "outfit"), SE = FALSE)

# S3 method for class 'PPfit'
print(x, digits = 4, ...)

# S3 method for class 'PPfit'
summary(object, alpha = 0.05, digits = 4, ...)
```

## Arguments

- respm:

  Not needed: the responses are taken from `pp`. If given, it must be
  the response matrix of `pp`.

- pp:

  An object of class `ppeo` from
  [`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md),
  [`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md) or
  [`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) with
  type `"mle"`, `"wle"` or `"map"`.

- fitindices:

  Character vector of the desired person fit statistics, any of
  `c("lz", "lzstar", "infit", "outfit")`.

- SE:

  Logical: if `TRUE`, jackknife standard errors are computed.

- x, object:

  An object of class `PPfit`.

- digits:

  Number of significant digits to print.

- ...:

  Further arguments, passed to
  [`print()`](https://rdrr.io/r/base/print.html).

- alpha:

  Significance level: persons with \\\|z\| \> z\_{1-\alpha/2}\\ (lz,
  lz\*, `in_t`, `ou_t`) are counted as flagged.

## Value

An object of class `PPfit`: a list with one matrix per statistic
(persons in rows, in the order of the response matrix):

- `lz`: `lz`, `lz_unst`

- `lzstar`: `lzstar`

- `infit`: `infit` (mean square), `in_t`, `in_chisq`, `in_df`, `in_pv`

- `outfit`: `outfit` (mean square), `ou_t`, `ou_chisq`, `ou_df`, `ou_pv`

- with `SE = TRUE` also `lz_se`, `lzs_se`, `in_se`, `ou_se`.

[`summary()`](https://rdrr.io/r/base/summary.html) gives, per statistic,
the number of persons, mean, SD, range and the number of persons flagged
at level `alpha`.

## Details

All statistics are computed at the estimate \\\hat\theta\\ of each
person, over the items the person answered (missing responses are left
out of every sum). With the observed response \\x_i\\, the expected
score \\E_i\\, its variance \\V_i\\ and fourth central moment \\C_i\\ of
item \\i\\, the residual is \\Y_i = x_i - E_i\\, and for the \\n\\
answered items:

- **infit** (weighted mean square, Wright & Masters, 1982): \\\sum Y_i^2
  / \sum V_i\\, with variance \\q^2 = \sum (C_i - V_i^2) / (\sum
  V_i)^2\\.

- **outfit** (unweighted mean square): \\\sum (Y_i^2 / V_i) / n\\, with
  variance \\q^2 = \sum (C_i / V_i^2) / n^2 - 1/n\\.

- Both mean squares \\MS\\ are standardised by the Wilson-Hilferty
  transformation \\t = (MS^{1/3} - 1) (3/q) + q/3\\. `chisq` is \\\sum
  Y_i^2 / V_i\\ with `df` \\= n - 1\\ and the p-value of the \\\chi^2\\
  distribution.

- **lz** (Drasgow, Levine & Williams, 1985): the log-likelihood \\l_0 =
  \sum \log P_i(x_i)\\, standardised by its expectation and variance
  under the model, \\(l_0 - \sum E \log P_i) / \sqrt{\sum Var \log
  P_i}\\. `lz_unst` is \\l_0 / n\\.

- **lz\*** (Snijders, 2001) corrects lz for the estimated \\\theta\\:
  with the item information \\I\\, \\c_n = \sum Cov(\log P_i, s_i) / I\\
  (\\s_i\\ the score of item \\i\\) and the term \\r_0\\ of the
  estimating equation (mle: 0, wle: \\J / (2I)\\, map: \\(\mu - \theta)
  / \sigma^2\\), lz\* \\= (l_0 - \sum E \log P_i + c_n r_0) / \sqrt{\sum
  Var \log P_i - c_n^2 I}\\. For dichotomous items this is Snijders'
  lz\*, for polytomous items and mixed tests the generalisation of
  Sinharay (2016).

Person fit needs an estimate of type `"mle"`, `"wle"` or `"map"`.
Persons without an estimate (status other than `"ok"` in `pp$estimates`,
e.g. an extreme score with mle) get `NA`. With a single answered
dichotomous item lz\* is not defined (the corrected variance is 0) and
the \\\chi^2\\ p-value has no degree of freedom; both are `NA`.

With `SE = TRUE` every statistic gets a jackknife standard error: for
each answered item \\j\\, \\\theta\\ is estimated without item \\j\\
(the leave-one-out estimates of
[`JKpp()`](https://manuelreif.github.io/PP/reference/Jkpp.md)) and the
statistic \\f\_{(-j)}\\ is computed on the remaining items. With \\L\\
answered items and \\m\\ leave-one-out samples with a finite estimate,
\\SE = (L - 1) \sqrt{\sum_j (f\_{(-j)} - \bar f)^2 / (m (m - 1))}\\,
which is \\\sqrt{(L-1)/L \sum_j (f\_{(-j)} - \bar f)^2}\\ when no sample
is left out (Efron & Stein, 1981).

Up to version 0.6.4, lz\* summed over all items also for persons with
missing responses, the GPCM infit and outfit used the observed instead
of the possible categories and ignored the slopes, mixed tests were not
supported, the jackknife standard error of lz used the prior SD as
variance, and all results were rounded.

## References

Armstrong, R. D., Stoumbos, Z. G., Kung, M. T. & Shi, M. (2007). On the
performance of the lz person-fit statistic. *Practical Assessment,
Research & Evaluation*, **12(16)**.

De La Torre, J., & Deng, W. (2008). Improving Person-Fit Assessment by
Correcting the Ability Estimate and Its Reference Distribution. Journal
of Educational Measurement, **45(2)**, 159-177.

Drasgow, F., Levine, M. V. & Williams, E. A. (1985) Appropriateness
measurement with polychotomous item response models and standardized
indices. *British Journal of Mathematical and Statistical Psychology*,
**38(1)**, 67–86.

Efron, B., & Stein, C. (1981). The jackknife estimate of variance. *The
Annals of Statistics*, **9(3)**, 586-596.

Karabatsos, G. (2003) Comparing the Aberrant Response Detection
Performance of Thirty-Six Person-Fit Statistics. *Applied Measurement In
Education*, **16(4)**, 277–298.

Magis, D., Raiche, G. & Beland, S. (2012) A didactic presentation of
Snijders's lz\* index of person fit with emphasis on response model
selection and ability estimation. *Journal of Educational and Behavioral
Statistics*, **37(1)**, 57–81.

Meijer, R. R. & Sijtsma, K. (2001) Methodology review: Evaluating person
fit. *Applied Psychological Measurement*, **25(2)**, 107–135.

Molenaar, I. W. & Hoijtink, H. (1990) The many null distributions of
person fit indices. *Psychometrika*, **55(1)**, 75–106.

Reise, S. P. (1990). A comparison of item-and person-fit methods of
assessing model-data fit in IRT. *Applied Psychological Measurement*,
**14(2)**, 127-137.

Sinharay, S. (2016). Asymptotically correct standardization of
person-fit statistics beyond dichotomous items. *Psychometrika*,
**81(4)**, 992–1013.

Snijders, T. A. B. (2001) Asymptotic null distribution of person fit
statistics with estimated person parameter. *Psychometrika*, **66(3)**,
331–342.

Wright, B. D. & Masters, G. N. (1990). Computation of OUTFIT and INFIT
Statistics. *Rasch Measurement Transactions*, 3:4, 84-85.

Wright, B. D., & Masters, G. N. (1982). *Rating Scale Analysis. Rasch
Measurement.* MESA Press, 5835 S. Kimbark Avenue, Chicago, IL 60637.

## See also

[PPall](https://manuelreif.github.io/PP/reference/PPall.md),
[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
[PPass](https://manuelreif.github.io/PP/reference/PPass.md),
[JKpp](https://manuelreif.github.io/PP/reference/Jkpp.md)

## Author

Jan Steinfeld, Manuel Reif

## Examples

``` r
## dichotomous items

set.seed(1337)
diffpar <- seq(-3, 3, length = 15)
sl      <- round(runif(15, 0.5, 1.5), 2)
la      <- round(runif(15, 0, 0.25), 2)
awm     <- sim_4pl(beta = diffpar, alpha = sl, lowerA = la, upperA = rep(1, 15),
                   theta = rnorm(100))

# 3PL, WLE
res3plwle <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, lowerA = la, type = "wle")

# the responses are taken from the estimation object
fit3pl <- Pfit(pp = res3plwle, fitindices = c("lz", "lzstar", "infit", "outfit"))
head(fit3pl$lzstar)
#>          lzstar
#> [1,]  0.5521459
#> [2,]  1.6813575
#> [3,] -0.4255557
#> [4,]  0.1175374
#> [5,] -1.1638317
#> [6,]  0.7252538
summary(fit3pl)
#> Person fit at the wle estimates, 100 persons
#> flagged: |z| > 1.96 (alpha = 0.05)
#> 
#>  statistic   n     mean     sd    min   max flagged
#>         lz 100  0.24446 0.9577 -3.236 2.042       5
#>     lzstar 100  0.22370 1.0748 -3.651 2.069       6
#>      infit 100  0.91977 0.2671  0.454 1.782      NA
#>       in_t 100 -0.28880 0.9898 -2.626 2.375       4
#>     outfit 100  0.94935 0.7122  0.214 5.281      NA
#>       ou_t 100 -0.03654 0.8714 -2.396 2.647       5

# with missing responses and jackknife standard errors
awm_na <- awm
awm_na[sample(length(awm), 300)] <- NA
res3plna <- PP_4pl(respm = awm_na, thres = diffpar, slopes = sl, lowerA = la, type = "wle")
head(Pfit(pp = res3plna, fitindices = c("lz", "infit"), SE = TRUE)$lz)
#>               lz    lz_unst     lz_se
#> [1,]  0.81668035 -0.3731788 0.5782741
#> [2,]  1.34918087 -0.3149013 0.3219197
#> [3,]  0.03583467 -0.2976251 0.6904146
#> [4,]  0.14251578 -0.3851398 0.5919541
#> [5,] -1.52450167 -0.7256653 1.7532009
#> [6,]  0.83950493 -0.3462636 0.4622144

## GPCM

thres <- rbind(0, c(-1, -0.5, 0), c(0.5, 0.8, 1), c(1.5, 2, 2.5))
slg   <- c(0.8, 1.2, 1)
respg <- sim_gpcm(thres = thres, alpha = slg, theta = rnorm(50))
resgwle <- PP_gpcm(respm = respg, thres = thres, slopes = slg, type = "wle")
summary(Pfit(pp = resgwle))
#> Person fit at the wle estimates, 50 persons
#> flagged: |z| > 1.96 (alpha = 0.05)
#> 
#>  statistic  n    mean     sd      min   max flagged
#>         lz 50  0.3494 0.7066 -1.58275 1.220       0
#>     lzstar 50  0.2513 0.8197 -1.75562 1.295       0
#>      infit 50  0.7419 0.6321  0.04879 2.578      NA
#>       in_t 50 -0.2864 0.9874 -2.40375 1.755       3
#>     outfit 50  0.7153 0.5945  0.04635 2.286      NA
#>       ou_t 50 -0.2945 0.9587 -2.43742 1.538       5

## GPCM and 4PL mixed

THRESx <- matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5), nrow = 2)
THRESx[2, 1:3] <- NA
slm <- c(0.5, 1, 1.5, 1.1, 1, 0.98)
lam <- c(0.02, 0.1, 0, 0, 0, 0)
uam <- c(0.97, 0.91, 1, 1, 1, 1)
awmatrix <- matrix(c(1, 0, 1, 0, 1, 1, 1, 0, 0, 1,
                     2, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                     1, 2, 2, 1, 1, 1, 1, 0, 0, 1), byrow = TRUE, nrow = 5)
resmixed <- PPall(respm = awmatrix, thres = THRESx, slopes = slm, lowerA = lam,
                  upperA = uam, type = "wle", model2est = findmodel(THRESx))
Pfit(pp = resmixed, fitindices = c("lz", "lzstar"))
#>           lz lz_unst   lzstar
#> [1,] -1.2497 -1.0037 -1.28788
#> [2,] -0.8544 -0.9200 -0.91438
#> [3,]  0.7053 -0.1317  0.05879
#> [4,] -0.3854 -0.6873 -1.11187
#> [5,] -1.5745 -1.0639 -1.65248
```

# Draw Plausible values

This function draws `npv` plausible values for each person from their
posterior density.

## Usage

``` r
# S3 method for class 'pv'
print(x, digits = 4, ...)

# S3 method for class 'pv'
summary(object, nrowmax = 15, digits = 4, ...)

# S3 method for class 'ppass'
PV(estobj, ...)

PV(estobj, ...)

# S3 method for class 'ppeo'
PV(estobj, npv = 10, approx = TRUE, ...)
```

## Arguments

- x, object:

  An object of class `pv`, the result of `PV()`.

- digits:

  Number of significant digits to print.

- ...:

  Not used. `thinning`, `burnin` and `mult` of version 0.6.4 give a
  warning and are ignored.

- nrowmax:

  [`summary()`](https://rdrr.io/r/base/summary.html): how many persons
  to show. Default = 15.

- estobj:

  An object which originates from using
  [`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md),
  [`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md) or
  [`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) with
  `type = "eap"` or `type = "map"`.

- npv:

  The number of plausible values per person - default is 10.

- approx:

  If `TRUE` (default), the plausible values are drawn from a normal
  approximation of the posterior. If `FALSE`, they are drawn from the
  posterior itself.

## Value

An object of class `pv`, a list with `pvdraws`, a matrix with one row
per person (in the order of `respm`) and `npv` columns, and `call`.

## Details

Plausible values need a prior, so `estobj` must come from an estimation
with `type = "eap"` or `type = "map"`. The posterior of each person
combines the likelihood of the answered items with the normal prior
`N(mu, sigma2)` of the person, as given to
[`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md) or
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) (default
`N(0, 1)`). It is computed on a grid of ability values, the same as for
`type = "eap"`: one grid for all persons, and a finer grid around the
posterior mode for persons whose posterior is very narrow or lies
outside the range of the prior. Both types give the same plausible
values, since they share the posterior; the point estimate itself is not
used. MLE, WLE and robust estimates have no prior, and `PV()` stops with
an error for them. Missing responses are skipped; a person without any
response gets draws from the prior.

With `approx = FALSE` each value is drawn from the posterior on the
grid: a grid point is chosen with its posterior probability, then the
value is drawn uniformly within its cell. The draws are independent.
With `approx = TRUE` (default) the draws come from the normal
distribution with the posterior mean and the posterior SD of the person
(the EAP estimate and its SE).

Up to version 0.6.4 `PV()` accepted any estimation type,
`approx = FALSE` used a Metropolis-Hastings algorithm and
`approx = TRUE` drew from the normal distribution around the estimate of
`estobj` with its SE. For an MLE or WLE estimate this is not a posterior
and the plausible values were too widely spread. The arguments
`thinning`, `burnin` and `mult` of the sampler are no longer used.

## References

Mislevy, R. J. (1991). Randomization-based inference about latent
variables from complex samples. Psychometrika, 56(2), 177-196.

Von Davier, M., Gonzalez, E., & Mislevy, R. (2009). What are plausible
values and why are they useful. IERI monograph series, 2, 9-36.

## See also

[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[PPall](https://manuelreif.github.io/PP/reference/PPall.md),
[JKpp](https://manuelreif.github.io/PP/reference/Jkpp.md)

## Author

Manuel Reif

## Examples

``` r
## 2PL model

set.seed(1522)
# difficulties and slopes
diffpar <- seq(-3, 3, length = 12)
sl      <- round(runif(12, 0.5, 1.5), 2)

# response matrix
awm <- sim_4pl(beta = diffpar, alpha = sl, lowerA = rep(0, 12), upperA = rep(1, 12),
               theta = rnorm(10))

# plausible values need a prior: EAP (or MAP) estimation, here with the default N(0, 1)
res2pl <- PP_4pl(respm = awm, thres = diffpar, slopes = sl, type = "eap")

# 10 plausible values per person, normal approximation of the posterior
res_pv <- PV(res2pl)
summary(res_pv)
#> PP Version: 0.7.0 
#> 
#> Call: PV.ppeo(estobj = res2pl) 
#> - job started @ Sun Oct 11 15:57:36 2026 
#> 
#> Plausible values: 10 per person, 10 persons
#> Mean 0.07516 | SD 0.9201 (all draws)
#> -------------------------------------
#>           PV1      PV2      PV3      PV4      PV5      PV6      PV7     PV8
#>  [1,]  0.1253 -0.35189 -0.02546 -1.15817 -0.18511 -0.60575  0.45155 -1.2220
#>  [2,] -0.7018  0.27624 -0.82530 -1.37445 -0.75688 -0.10974  0.16777 -1.0637
#>  [3,]  2.3971  2.23987  0.57572  1.02926  0.95906  1.90306  1.57050  1.9655
#>  [4,]  0.4075  0.08102 -0.17301  0.32869 -0.09849  1.05072 -0.41746  1.2029
#>  [5,]  0.4192 -0.44634 -0.87334 -0.08097 -0.43862  0.46827  0.13674  0.6669
#>  [6,]  1.0796  0.07405  0.48569 -0.19520  0.56975 -0.02776  0.42421  0.9394
#>  [7,] -1.2205 -0.56482 -0.87924 -2.01555  0.30109 -1.13720 -0.89102 -1.8827
#>  [8,] -0.8826  0.16860 -0.87608  0.08319 -0.36860 -0.38556 -0.62912  0.4336
#>  [9,]  0.4148 -0.43724 -0.29219  0.64643  0.80497  0.91551 -0.07344 -0.5810
#> [10,]  0.9809  0.06553  1.50630  1.46233  0.51602  0.50017  0.91457  0.3362
#>           PV9     PV10
#>  [1,] -0.3288 -0.86540
#>  [2,] -1.4350 -0.64277
#>  [3,]  0.9956  2.82550
#>  [4,]  0.2507 -0.04379
#>  [5,] -0.6744  0.71365
#>  [6,]  0.3493 -0.16149
#>  [7,] -1.9030 -0.45923
#>  [8,]  0.4542 -0.40521
#>  [9,]  0.6013 -0.49365
#> [10,] -0.5079  1.44878

# 200 draws from the posterior itself; their means approximate the EAP
res_pv2 <- PV(res2pl, npv = 200, approx = FALSE)
cbind(pv_mean = rowMeans(res_pv2$pvdraws),
      eap = res2pl$estimates$estimate)
#>            pv_mean         eap
#>  [1,] -0.135467142 -0.18583578
#>  [2,] -0.692377630 -0.65043569
#>  [3,]  1.364031350  1.41309311
#>  [4,]  0.141705847  0.19897822
#>  [5,] -0.009712871 -0.05293022
#>  [6,]  0.466294680  0.49475048
#>  [7,] -0.881990262 -0.92998457
#>  [8,] -0.338566825 -0.35522684
#>  [9,]  0.228345244  0.30678313
#> [10,]  0.708990902  0.73969097


## GPCM model with person-specific priors

THRES  <- matrix(c(-2,-1.23,1.11,3.48,1
                   ,2,-1,-0.2,0.5,1.3,-0.8,1.5),nrow=2)
sl     <- c(0.5,1,1.5,1.1,1,0.98)
awmatrix <- matrix(c(1,0,2,0,1,1,1,0,0,1
                     ,2,0,0,0,0,0,0,0,0,1,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)

resgpcm <- PP_gpcm(respm = awmatrix, thres = THRES, slopes = sl, type = "eap",
                   mu = c(0, 0, 1, 1, 1), sigma2 = 0.5)
PV(resgpcm, npv = 5, approx = FALSE)
#>          PV1      PV2     PV3      PV4     PV5
#> [1,] -0.1683  0.68916 -0.9263 -0.20318  1.1035
#> [2,] -0.7043  0.25510 -0.1691  0.33882 -0.4426
#> [3,] -0.9978 -1.49071 -0.8985 -0.34821 -0.9217
#> [4,]  1.9080  1.15721  1.2264  0.67310  1.1262
#> [5,]  0.6211  0.09289  0.2998  0.03677  0.4436


## GPCM and 4PL items mixed

THRESx <- THRES
THRESx[2,1:3] <- NA
la     <- c(0.02,0.1,0,0,0,0)
ua     <- c(0.97,0.91,1,1,1,1)
awmatrix <- matrix(c(1,0,1,0,1,1,1,0,0,1
                     ,2,0,0,0,0,0,0,0,0,1
                     ,1,2,2,1,1,1,1,0,0,1),byrow=TRUE,nrow=5)

respcm <- PPall(respm = awmatrix, thres = THRESx, slopes = sl, lowerA = la, upperA = ua,
                type = "eap", model2est = findmodel(THRESx))
res_mixedpv <- PV(respcm, npv = 200, approx = FALSE)

# quantiles of the draws per person
apply(res_mixedpv$pvdraws, 1, quantile)
#>             [,1]        [,2]       [,3]       [,4]         [,5]
#> 0%   -2.14555733 -1.90544670 -3.4545003 -0.3649307 -1.540995367
#> 25%  -0.34820871 -0.49531581 -2.0623500  0.8429533 -0.395656257
#> 50%   0.02380601 -0.03862204 -1.5547299  1.2243266  0.009660583
#> 75%   0.41264797  0.40474005 -1.0493188  1.7106722  0.439483367
#> 100%  1.73540987  1.86192629  0.6697426  3.2178725  1.344299865
```

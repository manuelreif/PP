# PP <img src="https://www.r-pkg.org/badges/version/PP" align="right" alt="CRAN version"/>

**PP** estimates person parameters (abilities) in item response theory when the item parameters are known, e.g. for items from an item bank given to a new sample.

- **Models:** 1PL (Rasch), 2PL, 3PL and 4PL model, partial credit and generalized partial credit model, and tests that mix dichotomous and polytomous items.
- **Estimates:** maximum likelihood, weighted likelihood (Warm), maximum a posteriori, expected a posteriori and a robust Huber-type estimate, each with standard error and a status per person.
- **More:** jackknife estimates, plausible values, the person fit statistics lz, lz\*, infit and outfit, and simulation of response data.
- **eRm:** `PPass()` takes models fitted with the [eRm](https://CRAN.R-project.org/package=eRm) package (RM, LLTM, RSM, PCM, LRSM, LPCM, also with time points and groups) and adds WLE/MAP/EAP estimates, person fit, jackknife and plausible values.

The numerical core is written in C++ and estimates the abilities of thousands of persons in milliseconds.

## Installation

```r
# from CRAN
install.packages("PP")

# development version from GitHub
# install.packages("remotes")
remotes::install_github("manuelreif/PP", build_vignettes = TRUE)
```

## Example

`fourpl_df` contains an id, the sex and the responses of 60 persons to 12 items of a 3PL test, with the item parameters as attributes. `PPass()` estimates the abilities (WLE by default), computes the person fit in the same step and keeps the other variables:

```r
library(PP)
data(fourpl_df)

res <- PPass(fourpl_df, items = 3:14, mod = "3PL",
             thres = attr(fourpl_df, "diffpar"), slopes = attr(fourpl_df, "slopes"),
             lowerA = attr(fourpl_df, "lowerA"))
print(res, n = 6)
#> Person assessment: 60 persons, 12 items
#> Model: 3PL model | WLE estimates
#> Status: 60 ok
#> Person fit: lz, lzstar, infit, outfit
#> Flagged (|lzstar| > 1.96): 4 persons
#>
#>        id sex estimate    SE status     lz lzstar infit   in_t outfit   ou_t
#> 1 LVL0694   w   -0.936 0.767     ok  1.019  1.026 0.692 -1.091 0.5342 -0.866
#> 2 BBU1225   w    1.811 1.022     ok  1.101  1.149 0.377 -1.537 0.2061 -0.526
#> 3 MJN2028   w    0.969 0.987     ok  1.270  1.324 0.288 -1.757 0.2011 -1.204
#> 4 TSU0771   m    1.224 1.008     ok -1.039 -1.157 1.487  0.978 1.9539  1.107
#> 5 XDS0698   w    0.252 0.925     ok  1.187  1.224 0.436 -1.392 0.3219 -1.257
#> 6 BOS1292   w   -4.730 2.118     ok  0.998  0.637 0.108 -1.505 0.0884 -1.196
#> ... 54 more persons
#> Not shown: lz_unst, in_chisq, in_df, in_pv, ou_chisq, ou_df, ou_pv
```

The result is a data.frame. `summary(res)` lists the flagged persons, and `JKpp(res)` works on it directly (as does `PV()` for EAP and MAP estimates). The estimation functions themselves are `PP_4pl()`, `PP_gpcm()` and `PPall()`:

```r
res_eap <- PP_4pl(respm = as.matrix(fourpl_df[, 3:14]),
                  thres = attr(fourpl_df, "diffpar"), slopes = attr(fourpl_df, "slopes"),
                  lowerA = attr(fourpl_df, "lowerA"), type = "eap")
head(res_eap$estimates)
```

## Learn more

- Vignettes: `vignette("intro_pp", package = "PP")`, `vignette("intro_pfit", package = "PP")` and `vignette("ppass_erm", package = "PP")`
- Website: <https://manuelreif.github.io/PP/>
- Changes: [NEWS.md](NEWS.md). Version 0.7.0 corrects several results of earlier versions (jackknife standard errors, MAP standard errors, plausible values, person fit with missing responses).
- Bugs and questions: <https://github.com/manuelreif/PP/issues>

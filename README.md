# PP <img src="https://www.r-pkg.org/badges/version/PP" align="right" alt="CRAN version"/>

**PP** estimates person parameters (abilities) in item response theory when the item parameters are known, e.g. for items from an item bank given to a new sample.

- **Models:** 1PL (Rasch), 2PL, 3PL and 4PL model, partial credit and generalized partial credit model, and tests that mix dichotomous and polytomous items.
- **Estimates:** maximum likelihood, weighted likelihood (Warm), maximum a posteriori, expected a posteriori and a robust Huber-type estimate, each with standard error and a status per person.
- **More:** jackknife estimates, plausible values, the person fit statistics lz, lz\*, infit and outfit, and simulation of response data.
- **eRm:** `PPass()` takes models fitted with the [eRm](https://CRAN.R-project.org/package=eRm) package (RM, LLTM, RSM, PCM, LRSM, LPCM, also with time points and groups) and adds WLE/MAP/EAP estimates, person fit, jackknife and plausible values.

The numerical core is written in C++ and estimates thousands of persons in milliseconds.

## Installation

```r
# from CRAN
install.packages("PP")

# development version from GitHub
# install.packages("remotes")
remotes::install_github("manuelreif/PP", build_vignettes = TRUE)
```

## Example

`fourpl_df` contains an id, the sex and the responses of 60 persons to 12 items of a 2PL test, with the item parameters as attributes. `PPass()` estimates the abilities (WLE by default) and the person fit in one step and keeps the other variables:

```r
library(PP)
data(fourpl_df)

res <- PPass(fourpl_df, items = 3:14, mod = "2PL",
             thres = attr(fourpl_df, "diffpar"), slopes = attr(fourpl_df, "slopes"))
print(res, n = 6)
#> Person assessment: 60 persons, 12 items
#> Model: 2PL model | WLE estimates
#> Status: 60 ok
#> Person fit: lz, lzstar, infit, outfit
#> Flagged (|lzstar| > 1.96): 7 persons
#>
#>        id sex estimate    SE status     lz  lzstar infit   in_t outfit   ou_t
#> 1 LVL0694   w   -0.879 0.726     ok  0.852  0.8512 0.732 -0.922  0.498 -0.221
#> 2 BBU1225   w    1.907 0.942     ok  1.081  1.1033 0.361 -1.494  0.194 -0.497
#> 3 MJN2028   w    1.082 0.917     ok  1.189  1.2115 0.279 -1.640  0.182 -1.139
#> 4 TSU0771   m    1.781 0.941     ok -1.515 -1.5476 1.631  1.173  3.157  1.534
#> 5 XDS0698   w    0.327 0.868     ok  1.122  1.1337 0.413 -1.359  0.275 -1.064
#> 6 BOS1292   w   -4.655 1.726     ok  0.622 -0.0483 0.109 -0.731  0.039  2.343
#> ... 54 more persons
#> Not shown: lz_unst, in_chisq, in_df, in_pv, ou_chisq, ou_df, ou_pv
```

The result is a data.frame; `summary(res)` lists the flagged persons, `JKpp(res)` and `PV()` work on it directly. The estimation functions themselves are `PP_4pl()`, `PP_gpcm()` and `PPall()`:

```r
res_eap <- PP_4pl(respm = as.matrix(fourpl_df[, 3:14]),
                  thres = attr(fourpl_df, "diffpar"), slopes = attr(fourpl_df, "slopes"),
                  type = "eap")
head(res_eap$estimates)
```

## Learn more

- Vignettes: `vignette("intro_pp", package = "PP")`, `vignette("intro_pfit", package = "PP")` and `vignette("ppass_erm", package = "PP")`
- Website: <https://manuelreif.github.io/PP/>
- Changes: [NEWS.md](NEWS.md). Version 0.7.0 corrects several results of earlier versions (jackknife standard errors, MAP standard errors, plausible values, person fit with missing responses).
- Bugs and questions: <https://github.com/manuelreif/PP/issues>

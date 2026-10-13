# PP: Person Parameter Estimation

The PP package estimates person parameters (abilities) in item response
theory when the item parameters are known and fixed, e.g. for items from
an item bank given to a new sample of persons.

## Details

- Models:

  the 1PL (Rasch), 2PL, 3PL and 4PL model
  ([`PP_4pl`](https://manuelreif.github.io/PP/reference/PP_4pl.md)), the
  partial credit and generalized partial credit model
  ([`PP_gpcm`](https://manuelreif.github.io/PP/reference/PP_gpcm.md))
  and tests that mix both
  ([`PPall`](https://manuelreif.github.io/PP/reference/PPall.md)).

- Estimates:

  maximum likelihood (MLE), weighted likelihood (WLE; Warm, 1989),
  maximum a posteriori (MAP), expected a posteriori (EAP) and a robust
  Huber-type estimate, each with a standard error and a status per
  person.

- Further analyses:

  jackknife estimates and standard errors
  ([`JKpp`](https://manuelreif.github.io/PP/reference/Jkpp.md)),
  plausible values
  ([`PV`](https://manuelreif.github.io/PP/reference/PV.md)) and the
  person fit indices lz, lz\*, infit and outfit
  ([`Pfit`](https://manuelreif.github.io/PP/reference/pfit.md)).

- In one step:

  [`PPass`](https://manuelreif.github.io/PP/reference/PPass.md)
  estimates and computes the person fit for a data.frame with item
  responses and other person variables, or for a model fitted with the
  eRm package (RM, LLTM, RSM, PCM, LRSM, LPCM).

- Simulation:

  [`sim_4pl`](https://manuelreif.github.io/PP/reference/sim_4pl.md),
  [`sim_gpcm`](https://manuelreif.github.io/PP/reference/sim_gpcm.md)
  and
  [`sim_mixed`](https://manuelreif.github.io/PP/reference/sim_mixed.md).

The numerical core is written in C++. Every person is estimated with a
safeguarded Newton method, EAP estimates and plausible values come from
the posterior on a grid. The vignettes give an introduction:
[`vignette("intro_pp", package = "PP")`](https://manuelreif.github.io/PP/articles/intro_pp.md),
[`vignette("intro_pfit", package = "PP")`](https://manuelreif.github.io/PP/articles/intro_pfit.md)
and
[`vignette("ppass_erm", package = "PP")`](https://manuelreif.github.io/PP/articles/ppass_erm.md).

## References

Barton, M. A., & Lord, F. M. (1981). An upper asymptote for the
three-parameter logistic item-response model. *ETS Research Report
Series*, 1981(1), i–8.

Birnbaum, A. (1968). Some latent trait models and their use in inferring
an examinee's ability. In F. M. Lord & M. R. Novick (Eds.), *Statistical
theories of mental test scores*. Reading, MA: Addison-Wesley.

Drasgow, F., Levine, M. V., & Williams, E. A. (1985). Appropriateness
measurement with polychotomous item response models and standardized
indices. *British Journal of Mathematical and Statistical Psychology*,
**38**(1), 67–86.

Muraki, E. (1992). A generalized partial credit model: Application of an
EM algorithm. *Applied Psychological Measurement*, **16**, 159–176.

Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability
in item response models. *Journal of Educational and Behavioral
Statistics*, **36**(6), 720–735.

Snijders, T. A. B. (2001). Asymptotic null distribution of person fit
statistics with estimated person parameter. *Psychometrika*, **66**(3),
331–342.

Wainer, H., & Wright, B. D. (1980). Robust estimation of ability in the
Rasch model. *Psychometrika*, **45**(3), 373–391.

Warm, T. A. (1989). Weighted likelihood estimation of ability in item
response theory. *Psychometrika*, **54**, 427–450.

## See also

Useful links:

- <https://github.com/manuelreif/PP>

- <https://manuelreif.github.io/PP/>

- Report bugs at <https://github.com/manuelreif/PP/issues>

## Author

**Maintainer**: Manuel Reif <manuel.reif@gmail.com>

Authors:

- Manuel Reif <manuel.reif@gmail.com>

- Jan Steinfeld <jan.d.steinfeld@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-9853-8260))

## Examples

``` r
# 12 items of a 2PL test with known parameters, 100 persons
set.seed(1522)
b  <- seq(-3, 3, length = 12)
sl <- round(runif(12, 0.5, 1.5), 2)
x  <- sim_4pl(beta = b, alpha = sl, lowerA = 0, upperA = 1, theta = rnorm(100))

res <- PP_4pl(respm = x, thres = b, slopes = sl, type = "wle")
head(res$estimates)
#>     estimate        SE nsteps status
#> 1 -0.9304929 0.8369461      4     ok
#> 2 -2.7893053 0.8908146      5     ok
#> 3  3.2224542 1.0747279      5     ok
#> 4  0.1683472 0.7848443      3     ok
#> 5 -1.6555101 0.8532582      4     ok
#> 6  1.2207987 0.7839490      4     ok

# estimates and person fit in one table
PPass(data.frame(id = 1:100, x), items = 2:13, thres = b, slopes = sl)
#> Person assessment: 100 persons, 12 items 
#> Model: 2PL model | WLE estimates
#> Status: 100 ok 
#> Person fit: lz, lzstar, infit, outfit 
#> Flagged (|lzstar| > 1.96): 1 persons
#> 
#>    id estimate    SE status     lz lzstar infit   in_t outfit   ou_t
#> 1   1   -0.930 0.837     ok -2.167 -2.248 1.883  1.863  2.200  1.301
#> 2   2   -2.789 0.891     ok  0.689  0.751 0.678 -0.712  0.316  0.371
#> 3   3    3.222 1.075     ok  0.785  0.885 0.513 -0.821  0.209  1.707
#> 4   4    0.168 0.785     ok -0.659 -0.664 1.206  0.659  1.309  0.613
#> 5   5   -1.656 0.853     ok  0.902  0.935 0.597 -0.968  0.343 -0.290
#> 6   6    1.221 0.784     ok -0.805 -0.837 1.349  0.969  1.115  0.601
#> 7   7   -0.109 0.796     ok  0.269  0.265 0.949 -0.020  0.664 -0.191
#> 8   8    0.350 0.780     ok  0.548  0.548 0.792 -0.506  0.644 -0.103
#> 9   9   -0.599 0.820     ok  0.688  0.694 0.749 -0.580  0.488 -0.504
#> 10 10   -0.367 0.808     ok -0.454 -0.474 1.052  0.260  1.700  0.990
#> ... 90 more persons
#> Not shown: lz_unst, in_chisq, in_df, in_pv, ou_chisq, ou_df, ou_pv 
```

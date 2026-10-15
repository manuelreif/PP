# Estimate Person Parameters for the 4-PL model

Compute Person Parameters for the 1/2/3/4-PL model and choose between
five common estimation techniques: ML, WL, MAP, EAP and a robust
estimation. All item parameters are treated as fixed.

## Usage

``` r
PP_4pl(
  respm,
  thres,
  slopes = NULL,
  lowerA = NULL,
  upperA = NULL,
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

  A numeric vector or a numeric matrix which contains the threshold
  parameter (also known as difficulty parameter or beta parameter) for
  each item. A matrix with one column per item is also accepted; its
  first row of zeros (as in
  [`PP_gpcm`](https://manuelreif.github.io/PP/reference/PP_gpcm.md)) is
  optional.

- slopes:

  A numeric vector, which contains the slope parameters for each item -
  one parameter per item is expected. A single value is used for all
  items. If `NULL` (default), all slopes are 1.

- lowerA:

  A numeric vector, which contains the lower asymptote parameters (kind
  of guessing parameter) for each item. A single value is used for all
  items. If `NULL` (default), all lower asymptotes are 0.

- upperA:

  numeric vector, which contains the upper asymptote parameters for each
  item. A single value is used for all items. If `NULL` (default), all
  upper asymptotes are 1.

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

With this function you can estimate:

- **1-PL model** (Rasch model) by submitting: the data matrix, item
  difficulties and **nothing else**, since the 1-PL model is merely a
  4-PL model with: any slope = 1, any lower asymptote = 0 and any upper
  asymptote = 1!

- **2-PL model** by submitting: the data matrix, item difficulties and
  slope parameters. Lower and upper asymptotes are automatically set to
  0 and 1 respectively.

- **3-PL model** by submitting anything except the upper asymptote
  parameters

- **4-PL model** by submitting all parameters.

The probability function of the 4-PL model is: \$\$P(x\_{ij} = 1 \| \hat
\alpha_i, \hat\beta_i, \hat\gamma_i, \hat\delta_i, \theta_j ) =
\hat\gamma_i + (\hat\delta_i-\hat\gamma_i) \frac{exp(\hat \alpha_i
(\theta\_{j} - \hat\beta\_{i}))}{\\1 + exp(\hat\alpha_i (\theta\_{j} -
\hat\beta\_{i}))}\$\$

In our case \\\theta\\ is to be estimated, and the four item parameters
are assumed as fixed (usually these are estimates of a former scaling
procedure).

The 3-PL model is the same, except that \\\delta_i = 1, \forall i\\.

In the 2-PL model \\\delta_i = 1, \gamma_i = 0, \forall i\\.

In the 1-PL model \\\delta_i = 1, \gamma_i = 0, \alpha_i = 1, \forall
i\\.

The **robust** estimation method, applies a Huber-type estimator
(Schuster & Yuan, 2011), which downweights responses to items which
provide little information for the ability estimation. First a residuum
is estimated and on this basis, the weight for each observation is
computed.

residuum: \$\$r_i = \alpha_i(\theta - \beta_i)\$\$

weight:

\$\$w(r_i) = 1 \rightarrow if\\ \|r_i\| \leq H\$\$ \$\$w(r_i) = H/\|r\|
\rightarrow if\\ \|r_i\| \> H\$\$

## References

Baker, Frank B., and Kim, Seock-Ho (2004). Item Response Theory -
Parameter Estimation Techniques. CRC-Press.

Barton, M. A., & Lord, F. M. (1981). An Upper Asymptote for the
Three-Parameter Logistic Item-Response Model.

Birnbaum, A. (1968). Some latent trait models and their use in inferring
an examinee's ability. In Lord, F.M. & Novick, M.R. (Eds.), Statistical
theories of mental test scores. Reading, MA: Addison-Wesley.

Magis, D. (2013). A note on the item information function of the
four-parameter logistic model. Applied Psychological Measurement, 37(4),
304-315.

Samejima, Fumiko (1993). The bias function of the maximum likelihood
estimate of ability for the dichotomous response level. Psychometrika,
58, 195-209.

Samejima, Fumiko (1993). An approximation of the bias function of the
maximum likelihood estimate of a latent variable for the general case
where the item responses are discrete. Psychometrika, 58, 119-138.

Schuster, C., & Yuan, K. H. (2011). Robust estimation of latent ability
in item response models. Journal of Educational and Behavioral
Statistics, 36(6), 720-735.

Warm, Thomas A. (1989). Weighted Likelihood Estimation Of Ability In
Item Response Theory. Psychometrika, 54, 427-450.

Yen, Y.-C., Ho, R.-G., Liao, W.-W., Chen, L.-J., & Kuo, C.-C. (2012). An
empirical evaluation of the slip correction in the four parameter
logistic models with computerized adaptive testing. Applied
Psychological Measurement, 36, 75-87.

## See also

[PPass](https://manuelreif.github.io/PP/reference/PPass.md),
[PPall](https://manuelreif.github.io/PP/reference/PPall.md),
[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
[JKpp](https://manuelreif.github.io/PP/reference/Jkpp.md),
[PV](https://manuelreif.github.io/PP/reference/PV.md)

## Author

Manuel Reif

## Examples

``` r
## real data: an adaptive intelligence test with known item difficulties (1PL)
data(pp_amt)
d <- as.matrix(pp_amt$daten_amt[, -(1:7)])
res <- PP_4pl(respm = d, thres = pp_amt$betas[, 2], type = "wle")
summary(res)
#> PP Version: 1.0.0 
#> 
#> Call: PP_4pl(respm = d, thres = pp_amt$betas[, 2], type = "wle") 
#> - job started @ Thu Oct 15 18:35:21 2026 
#> 
#> Estimation type: wle 
#> 
#> Status of the persons:
#> 
#>            ok       extreme      boundary   no_response not_converged 
#>           710             0             0             0             0 
#> 
#> Newton steps: at most 7 
#> -------------------------------------
#>    estimate     SE nsteps status
#> 1    1.4099 0.4178      4     ok
#> 2    0.3516 0.3729      3     ok
#> 3    1.0745 0.3741      4     ok
#> 4    0.3204 0.6900      3     ok
#> 5    1.3880 0.4644      3     ok
#> 6    0.9457 0.4185      4     ok
#> 7    1.8276 0.4108      4     ok
#> 8   -0.2274 0.3964      4     ok
#> 9    1.4426 0.4074      4     ok
#> 10   1.6430 0.4033      4     ok
#> 11  -0.4802 0.4759      3     ok
#> 12  -0.3591 0.4086      3     ok
#> 13  -0.1911 0.4137      3     ok
#> 14   0.5781 0.4177      3     ok
#> 15  -0.8898 0.4215      4     ok
#> --------> output truncated <--------

## simulated data: 2PL, 3PL and 4PL
set.seed(1522)
b  <- seq(-3, 3, length = 12)
sl <- round(runif(12, 0.5, 1.5), 2)
la <- round(runif(12, 0, 0.25), 2)
ua <- round(runif(12, 0.8, 1), 2)
x  <- sim_4pl(beta = b, alpha = sl, lowerA = la, upperA = ua, theta = rnorm(50))

res2pl <- PP_4pl(respm = x, thres = b, slopes = sl, type = "mle")
res3pl <- PP_4pl(respm = x, thres = b, slopes = sl, lowerA = la, type = "wle")
res4pl <- PP_4pl(respm = x, thres = b, slopes = sl, lowerA = la, upperA = ua, type = "map")

# persons with all answers right (or wrong) have no finite ML estimate
table(res2pl$estimates$status)
#> 
#>            ok       extreme      boundary   no_response not_converged 
#>            50             0             0             0             0 
head(res3pl$estimates)
#>     estimate        SE nsteps status
#> 1  0.2323310 0.9439296      2     ok
#> 2 -0.4027227 1.0044541      2     ok
#> 3 -2.2338872 1.0789771      8     ok
#> 4  0.4016054 0.9324362      4     ok
#> 5  0.8125907 0.9142177      4     ok
#> 6  0.1237144 0.9524919      4     ok
coef(res4pl)[1:5]
#> [1]  0.23729052  0.01516983 -0.98313162  0.22643916  0.63615728

## EAP with person-specific priors
res_eap <- PP_4pl(respm = x, thres = b, slopes = sl, type = "eap",
                  mu = rep(c(-0.5, 0.5), 25), sigma2 = 0.8)

## robust estimation: the example of Schuster & Yuan (2011)
AWM <- matrix(0, 7, 7)
diag(AWM) <- 1
coef(PP_4pl(respm = AWM, thres = -3:3, type = "mle"))
#> [1] -2.94088 -2.94088 -2.94088 -2.94088 -2.94088 -2.94088 -2.94088
coef(PP_4pl(respm = AWM, thres = -3:3, type = "robust"))
#> [1] -2.746716 -2.746657 -4.039526 -4.254544 -4.417379 -4.548895 -4.659856
```

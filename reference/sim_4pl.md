# Simulate data for 1/2/3/4-pl model

This function returns a dichotomous matrix of simulated responses under
given item and person parameters.

## Usage

``` r
sim_4pl(beta, alpha, lowerA, upperA, theta)
```

## Arguments

- beta:

  A numeric vector which contains the difficulty parameters for each
  item.

- alpha:

  A numeric vector, which contains the slope parameters for each item.

- lowerA:

  A numeric vector, which contains the lower asymptote parameters (kind
  of guessing parameter) for each item.

- upperA:

  numeric vector, which contains the upper asymptote parameters for each
  item.

- theta:

  A numeric vector which contains person parameters.

## Value

An integer matrix with one row per person and one column per item.

## Details

`alpha`, `lowerA` and `upperA` may also be single numbers, which are
then used for every item. A person with `NA` as person parameter gets a
row of `NA`.

## See also

[sim_gpcm](https://manuelreif.github.io/PP/reference/sim_gpcm.md),
[sim_mixed](https://manuelreif.github.io/PP/reference/sim_mixed.md),
[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md)

## Author

Manuel Reif

## Examples

``` r
set.seed(1700)
theta <- rnorm(200)
b <- seq(-2, 2, length.out = 7)

# 1PL (Rasch) model: slopes 1, no asymptotes
x1 <- sim_4pl(beta = b, alpha = 1, lowerA = 0, upperA = 1, theta = theta)
colMeans(x1)
#> [1] 0.820 0.755 0.700 0.430 0.365 0.265 0.120

# 4PL model
x4 <- sim_4pl(beta = b, alpha = c(1, 1.1, 0.9, 0.9, 1.5, 1.1, 1),
              lowerA = c(0, 0, 0.2, 0.15, 0.04, 0, 0.21),
              upperA = c(0.9, 0.98, 0.97, 1, 1, 1, 0.97), theta = theta)
head(x4)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    0    1    1    1    0    0    1
#> [2,]    1    1    1    0    0    0    0
#> [3,]    0    1    1    1    0    0    0
#> [4,]    1    1    0    0    0    0    0
#> [5,]    1    0    1    0    1    0    1
#> [6,]    1    1    1    0    1    0    0
```

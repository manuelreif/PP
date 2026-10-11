# Simulate data for the gpcm model

This function returns an integer matrix of simulated responses under
given item and person parameters.

## Usage

``` r
sim_gpcm(thres, alpha, theta)
```

## Arguments

- thres:

  A numeric matrix which contains threshold parameters for each item.
  The first row may contain zeroes only (as in
  [`PP_gpcm`](https://manuelreif.github.io/PP/reference/PP_gpcm.md));
  otherwise a row of zeroes is added.

- alpha:

  A numeric vector, which contains the slope parameters - one parameter
  per item is expected.

- theta:

  A numeric vector which contains person parameters.

## Value

An integer matrix with one row per person and one column per item.

## Details

`alpha` may also be a single number, which is then used for every item.
A person with `NA` as person parameter gets a row of `NA`.

## See also

[sim_4pl](https://manuelreif.github.io/PP/reference/sim_4pl.md),
[sim_mixed](https://manuelreif.github.io/PP/reference/sim_mixed.md),
[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md)

## Examples

``` r
set.seed(1750)
# thresholds of six items with three categories (the row of zeroes is optional)
thres <- rbind(0, matrix(c(-2, -1.23, 1.11, 3.48, 1, 2, -1, -0.2, 0.5, 1.3, -0.8, 1.5),
                         nrow = 2))
x <- sim_gpcm(thres = thres, alpha = c(0.5, 1, 1.5, 1.1, 1, 0.98), theta = rnorm(100))
head(x)
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    0    0    0    0    0    1
#> [2,]    1    1    0    2    0    1
#> [3,]    1    0    1    2    1    1
#> [4,]    1    1    1    2    0    1
#> [5,]    2    1    0    2    2    2
#> [6,]    2    0    1    2    2    2

# the last two items get a fourth category
thres4 <- rbind(thres, c(NA, NA, NA, NA, 1.7, 1))
apply(sim_gpcm(thres = thres4, alpha = 1, theta = rnorm(100)), 2, max)
#> [1] 2 1 2 2 3 3
```

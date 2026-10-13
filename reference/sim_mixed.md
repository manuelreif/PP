# Simulate data for mixed tests (4PL and GPCM items)

This function returns an integer matrix of simulated responses for a
test with dichotomous (1/2/3/4-PL) and polytomous (GPCM) items, with the
item parameters in the layout of
[`PPall`](https://manuelreif.github.io/PP/reference/PPall.md).

## Usage

``` r
sim_mixed(thres, alpha = NULL, lowerA = NULL, upperA = NULL, theta)
```

## Arguments

- thres:

  A numeric matrix with the thresholds of each item in its columns, as
  in [`PPall`](https://manuelreif.github.io/PP/reference/PPall.md): for
  a 4PL item the difficulty, for a GPCM item its thresholds, `NA` below
  the last one. Items with one threshold are 4PL items, the others GPCM
  items. The first row may contain zeroes only; otherwise a row of
  zeroes is added.

- alpha:

  A numeric vector of slope parameters. If `NULL` (default), all slopes
  are 1.

- lowerA:

  A numeric vector of lower asymptotes (4PL items). If `NULL` (default),
  0.

- upperA:

  A numeric vector of upper asymptotes (4PL items). If `NULL` (default),
  1.

- theta:

  A numeric vector which contains person parameters.

## Value

An integer matrix with one row per person and one column per item.

## Details

`alpha`, `lowerA` and `upperA` may also be single numbers, which are
then used for every item. `lowerA` and `upperA` only apply to the 4PL
items. A person with `NA` as person parameter gets a row of `NA`. The
data of a pure 4PL or GPCM test are the same as those of
[`sim_4pl`](https://manuelreif.github.io/PP/reference/sim_4pl.md) or
[`sim_gpcm`](https://manuelreif.github.io/PP/reference/sim_gpcm.md) with
the same seed.

## See also

[sim_4pl](https://manuelreif.github.io/PP/reference/sim_4pl.md),
[sim_gpcm](https://manuelreif.github.io/PP/reference/sim_gpcm.md),
[PPall](https://manuelreif.github.io/PP/reference/PPall.md)

## Examples

``` r
set.seed(1800)
# four 3PL items and two GPCM items with three categories
thres <- rbind(0, c(-1, -0.3, 0.4, 1.2, -0.5, 0.2), c(NA, NA, NA, NA, 0.8, 1.4))
x <- sim_mixed(thres = thres, alpha = c(1, 1.2, 0.8, 1.1, 1, 0.9),
               lowerA = c(0.2, 0.1, 0.15, 0.2, 0, 0), theta = rnorm(300))
head(x)
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    0    1    0    0    2    0
#> [2,]    1    1    0    0    1    0
#> [3,]    1    0    0    0    2    1
#> [4,]    1    1    1    1    2    1
#> [5,]    1    0    1    1    1    0
#> [6,]    1    1    0    1    2    0

# and back: estimate the persons with the same parameters
res <- PPall(x, thres = thres, slopes = c(1, 1.2, 0.8, 1.1, 1, 0.9),
             lowerA = c(0.2, 0.1, 0.15, 0.2, 0, 0))
summary(res$estimates$estimate)
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -2.78386 -0.87766  0.01059 -0.05093  0.65864  3.22926 
```

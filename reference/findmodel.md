# The model of each item in a mixed test

This function shows which model
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) uses for
each item: an item with one threshold is a 4PL item, an item with
several thresholds a GPCM item.

## Usage

``` r
findmodel(thres)
```

## Arguments

- thres:

  A numeric matrix which contains the threshold parameter for each item.
  NA is allowed - in fact expected!

## Value

A character vector with one entry per item (column of `thres`): `"4PL"`
for items with one threshold, `"GPCM"` for items with more.

## Details

Up to version 0.6.4-1 its result was passed to
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) as
`model2est`. This is no longer needed:
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) derives
the model of each item from `thres` in the same way.

## See also

[PPall](https://manuelreif.github.io/PP/reference/PPall.md)

## Author

Manuel Reif

## Examples

``` r
thres <- rbind(0, c(-1, 0.5, -0.2), c(NA, 1.2, 0.4))
findmodel(thres)
#> [1] "4PL"  "GPCM" "GPCM"
```

# Create a model-type vector template

This is a small helper function which creates a vector template quick
and easily for the
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md)
function. Modify this template as you like.

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

This function tries to guess the model which was applied to each item by
using the matrix of threshold parameters. It only discriminates between
GPCM and 4-PL model, and returns a character vector of length equal to
the number of items, that contains `"GPCM"` or `"4PL"` entries depending
on the structure of the thres matrix.

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

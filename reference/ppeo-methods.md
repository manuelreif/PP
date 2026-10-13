# Methods for estimation results

Print, summarise and extract the person estimates of
[`PP_4pl()`](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[`PP_gpcm()`](https://manuelreif.github.io/PP/reference/PP_gpcm.md) and
[`PPall()`](https://manuelreif.github.io/PP/reference/PPall.md) (objects
of class `ppeo`). The plots are described in
[pp-plots](https://manuelreif.github.io/PP/reference/pp-plots.md).

## Usage

``` r
# S3 method for class 'ppeo'
print(x, digits = 4, ...)

# S3 method for class 'ppeo'
summary(object, nrowmax = 15, digits = 4, ...)

# S3 method for class 'ppeo'
coef(object, ...)

# S3 method for class 'ppeo'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x, object:

  An object of class `ppeo`.

- digits:

  Number of significant digits to print.

- ...:

  Further arguments, passed to
  [`print()`](https://rdrr.io/r/base/print.html).

- nrowmax:

  When printing the estimates - how many rows should be shown? Default =
  15.

- row.names, optional:

  Ignored; the rows are the persons in the order of the response matrix.

## Value

[`coef()`](https://rdrr.io/r/stats/coef.html) returns the estimates as a
vector (persons in the order of the response matrix),
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) the
data.frame `$estimates` (estimate, SE, nsteps, status).
[`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) return the object
invisibly.

## See also

[PP_4pl](https://manuelreif.github.io/PP/reference/PP_4pl.md),
[PP_gpcm](https://manuelreif.github.io/PP/reference/PP_gpcm.md),
[PPall](https://manuelreif.github.io/PP/reference/PPall.md),
[pp-plots](https://manuelreif.github.io/PP/reference/pp-plots.md)

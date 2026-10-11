# Simulated data set

This dataset contains the responses of 60 persons to 12 items of a 3PL
test, simulated with the
[`sim_4pl()`](https://manuelreif.github.io/PP/reference/sim_4pl.md)
function, together with an id and the sex of each person. The item
parameters are stored as attributes.

## Format

A data.frame with 60 rows and 14 columns:

- `id`:

  person id

- `sex`:

  sex of the person (`"m"` or `"w"`)

- `Item1`, ..., `Item12`:

  the responses (0 or 1, `NA` = not answered)

The item parameters are the attributes `diffpar` (difficulties),
`slopes`, `lowerA` (lower asymptotes) and `upperA` (upper asymptotes,
all 1), e.g. `attr(fourpl_df, "diffpar")`.

## Source

simulation

## See also

[sim_4pl](https://manuelreif.github.io/PP/reference/sim_4pl.md)

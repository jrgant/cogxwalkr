# Splitting methods

These functions handle split routines used in the estimation of
crosswalks. In general, we recommend using these functions via
[`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md),
but they are made available to the user who wishes to call them
directly.

- `make_unconditional_splits()` : implements the unconditional split
  routine

- `make_conditional_splits()` : implements the conditional split
  routine, in which splits in the data are defined by a binary variable
  (e.g., dementia) correlated with given cognitive measures (e.g., MMSE
  and MoCA) because the underlying cognitive construct is a common cause
  of all three

- `make_splits()` : a handler that checks the input dataset and passes
  it on to the appropriate split function with relevant arguments
  specified

## Usage

``` r
make_unconditional_splits(data, niter = NULL)

make_conditional_splits(cdvar = NULL, data, loop = FALSE)

make_splits(cdvar = NULL, data, cdloop = FALSE, niter = NULL)
```

## Arguments

- data:

  A data.table, data.frame, matrix, or list containing the cognitive
  measure data

- niter:

  Number of iterations to conduct for an unconditional split routine

- cdvar:

  Character string naming auxiliary variable by which to condition
  splits

- loop:

  Boolean declaring whether to use a for loop. The default FALSE will
  generate splits and operate on an expanded data.table in-memory. If
  your machine has limited memory, set this argument to TRUE in order to
  process splits sequentially. The default option should be much faster.

- cdloop:

  Boolean passed to `loop` argument of `make_conditional_splits()`.
  Ignored when conducting unconditional splits.

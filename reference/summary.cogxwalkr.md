# Summarize a cogxwalkr list

Summarize a cogxwalkr list

## Usage

``` r
# S3 method for class 'cogxwalkr'
summary(object, ..., alpha = 0.05, bci_type = c("percentile", "normal"))
```

## Arguments

- object:

  An object of class "cogxwalkr", i.e., as returned by
  [`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md).

- ...:

  Unused

- alpha:

  Alpha to use for confidence interval calculation. Defaults to 0.05.

- bci_type:

  Type of bootstrapped confidence interval to calculate. Currently
  accepts "percentile" (or "perc") and/or "normal." Both are included by
  default.

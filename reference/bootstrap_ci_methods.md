# Calculate bootstrapped confidence limits

Calculate bootstrapped confidence limits

## Usage

``` r
bootstrap_ci(cx, alpha = 0.05, type = "percentile")
```

## Arguments

- cx:

  An object of class "cogxwalkr", i.e., an object returned by
  [`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md).

- alpha:

  Alpha level to use for confidence interval calculation

- type:

  Type of confidence limits to calculate. Currently supported:
  percentile, normal.

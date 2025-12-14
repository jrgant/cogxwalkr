# Plot information about the bootstrap distribution

Plot information about the bootstrap distribution

## Usage

``` r
# S3 method for class 'cogxwalkr'
plot(
  x,
  ...,
  cxsum = NULL,
  types = c("boot", "slope"),
  breaks = "FD",
  citype = "percentile",
  layout = c(1, length(types)),
  sargs = list(col = "black", lty = 1, lwd = 2),
  bargs = list(col = "red", lty = 2, lwd = 2),
  slargs = list(col = "red", lwd = 2),
  clargs = list(col = "red", lty = 2),
  ptshape = 19,
  ptsize = 0.8,
  ptcol = "black",
  ptalpha = 0.2,
  lcex = 1
)
```

## Arguments

- x:

  An object of class "cogxwalkr", i.e., as returned by
  [`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md).

- ...:

  Pass arguments to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`hist()`](https://rdrr.io/r/graphics/hist.html). Note that both of
  these functions will inherit the additional parameters.

- cxsum:

  The output of `summary(cx)`

- types:

  The types of crosswalk plots to produce. By default, both a plot of
  the bootstrap distribution of coefficients and a plot of the data with
  the estimated slope. For the plotted slope, (1-alpha)% confidence
  intervals will appear if the user provides the output of
  [`summary.cogxwalkr()`](https://jrgant.github.io/cogxwalkr/reference/summary.cogxwalkr.md).

- breaks:

  Passed to [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html),
  overriding the default method with "FD"
  ([`grDevices::nclass.FD()`](https://rdrr.io/r/grDevices/nclass.html)).

- citype:

  Choose confidence intervals to plot. Ignored if `cxsum` is NULL.

- layout:

  Passed to the `mfrow` argument of
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html). Defaults to
  `c(1, length(types))`.

- sargs:

  List of parameters passed to the
  [`graphics::abline()`](https://rdrr.io/r/graphics/abline.html) that
  plots the sample coefficient estimate

- bargs:

  List of parameters passed to the
  [`graphics::abline()`](https://rdrr.io/r/graphics/abline.html) that
  plots the mean coefficient estimate across bootstrap replicates

- slargs:

  List of parameters passed to the
  [`graphics::abline()`](https://rdrr.io/r/graphics/abline.html) that
  plots the sample slope in the crosswalk panel

- clargs:

  List of parameters passed to the
  [`graphics::abline()`](https://rdrr.io/r/graphics/abline.html) calls
  that plot the confidence limits for the slope.

- ptshape:

  Shape of points in crosswalk scatterplot (passed to
  [`base::plot()`](https://rdrr.io/r/base/plot.html))

- ptsize:

  Size of points in crosswalk scatterplot (passed to
  [`base::plot()`](https://rdrr.io/r/base/plot.html))

- ptcol:

  Color of points in crosswalk scatterplot (passed to
  [`base::plot()`](https://rdrr.io/r/base/plot.html))

- ptalpha:

  Alpha (transparency) of points in crosswalk scatterplot (passed to
  [`base::plot()`](https://rdrr.io/r/base/plot.html))

- lcex:

  Scale legend.

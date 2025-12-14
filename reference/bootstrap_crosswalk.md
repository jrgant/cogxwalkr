# Bootstrap a split routine

Bootstrap a split routine

## Usage

``` r
bootstrap_crosswalk(..., nboot, ncores = 1L, seed)
```

## Arguments

- ...:

  Pass arguments to
  [`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md)

- nboot:

  Number of bootstrap replicates to generate

- ncores:

  Number of cores to use in parallel processing. If set to 999,
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html)
  will use the maximum available

- seed:

  Seed used by doRNG to generate reproducible parallel computations

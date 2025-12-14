# Bootstrap control list

Bootstrap control list

## Usage

``` r
boot_control(nboot = NULL, seed = NULL, ncores = 1L)
```

## Arguments

- nboot:

  Number of bootstrap replicates to produce.

- seed:

  Seed used to initialize parallel processing-safe random number
  generation.

- ncores:

  Number of cores to use in parallel processing. Defaults to 1.

## Details

Can be used as an argument to the `control` argument in
[`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md),
but intended primarily to validate the list provided by that argument.

# Crosswalk an effect estimate

Take a published effect estimate (e.g., the difference in Mini Mental
State Exam score comparing APOE-ε4 carriers to non-carriers) and
translate that effect estimate to an alternate scale (e.g., Montreal
Cognitive Assessment). The translation uses a crosswalk estimated via
[`crosswalk()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md)
in data where both measures are available.

## Usage

``` r
do_crosswalk(
  object,
  est_mean = NULL,
  est_se = NULL,
  est_ci = NULL,
  est_pval = NULL,
  est_alpha = 0.05,
  est_indep = NULL,
  est_outcome = NULL,
  alpha = 0.05
)
```

## Arguments

- object:

  An object of class `cogxwalkr` or the result of
  [`est_cw_coef()`](https://jrgant.github.io/cogxwalkr/reference/crosswalk.md)

- est_mean:

  Point estimate (beta) to be crosswalked to the alternative outcome
  measure

- est_se:

  The standard error of `est_mean`

- est_ci:

  The lower (1-alpha)% confidence interval of `est_mean`

- est_pval:

  The p-value corresponding to `est_mean`

- est_alpha:

  The alpha level for the confidence interval (if `est_se` is provided)
  or the alpha level that will be used to back-calculate the standard
  error from `est_ci`. Defaults to 0.05.

- est_indep:

  The independent variable to which `est_mean` applies

- est_outcome:

  The outcome measure in the original study (e.g., "MOCA", "MMSE")

- alpha:

  The alpha level for the confidence interval of the crosswalked
  estimate. Defaults to 0.05.

## Details

Parameters prefixed with `est_` refer to a summary estimate for which
the user lacks access to the underlying data but wishes to translate the
estimate to another cognitive measure's scale. The user must supply
`est_mean` and one of `est_se`, `est_ci`, or `est_pval`.
`do_crosswalk()` will back-calculate the standard error if necessary, as
follows:

- `est_ci` : `(confidence interval width) / 2 / (critical value)`, where
  "critical value" refers to the Z-value of the standard normal
  distribution assuming a two-sided `est_alpha`

- `est_pval` : `est_mean / (critical value)`, where "critical value" in
  this case is calculated assuming a two-sided p-value

As in the [Cochrane
Handbook](https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-06#section-6-3-1)
summary of these calculations, the function assumes that statistical
estimates for difference measures were calculated using the standard
normal distribution rather than a t-distribution.

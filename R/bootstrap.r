#' Bootstrap a split routine
#'
#' @param ... Pass arguments to `crosswalk()`
#' @param num_boot Number of bootstrap replicates to generate
#' @param num_cores Number of cores to use in parallel processing
#' @param rng_seed Seed used by doRNG to generate reproducible parallel computations
#' @param alpha Alpha level for bootstrap confidence intervals
#'
#' @import data.table
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @export

## FIXME: [2025-04-28] : conditional bootstrapping appears to give more precise
##   results than Sarah's paper
bootstrap_crosswalk <- function(..., num_boot, num_cores = 1, rng_seed) {
  dcopy <- as.data.table(eval(match.call()$data))
  if (num_cores == 1) {
    message("`num_cores` is set to 1. Parallel processing will not be used.")
  }

  registerDoParallel(num_cores)
  split_data <- foreach(i = seq_len(num_boot),
                        .inorder = FALSE,
                        .combine = c,
                        .options.RNG = rng_seed) %dorng% {
    data <- dcopy[sample(seq_len(.N), replace = TRUE)]
    tmp <- crosswalk(...)
    coef(tmp$fit)
  }
  stopImplicitCluster()

  split_data
}


#' Percentile method
#' @param bootdist A vector containing the bootstrap distribution
#' @param alpha Alpha level to use for confidence interval calculation
#'
#' @rdname bootstrap_ci_methods
percentile_bootstrap_ci <- function(bootdist, alpha = 0.05) {
  ql <- quantile(bootdist, c(alpha / 2, 1 - (alpha / 2)))
  citab <- data.table(method = "percentile",
                      alpha = alpha,
                      ll = ql[1],
                      ul = ql[2],
                      se = sd(bootdist))
  citab
}

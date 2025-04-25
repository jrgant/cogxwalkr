#' Bootstrap a split routine
#'
#' @param ... Pass arguments to `crosswalk()`
#' @param num_boot Number of bootstrap replicates to generate
#' @param num_cores Number of cores to use in parallel processing
#' @param rng_seed Seed used by doRNG to generate reproducible parallel computations
#' @param alpha Alpha level for bootstrap confidence intervals
#' @param sample_est The coefficient from a split procedure conducted in the original
#'
#' @import data.table
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @export
bootstrap_crosswalk <- function(..., num_boot, num_cores = 1, rng_seed,
                                alpha = 0.05, sample_est = NULL) {
  registerDoParallel(num_cores)
  split_data <- foreach(i = seq_len(num_boot),
                        .inorder = FALSE,
                        .combine = c,
                        .options.RNG = rng_seed) %dorng% {
    tmp <- crosswalk(...)
    coef(tmp$fit)
  }
  stopImplicitCluster()

  percentile_bootstrap_ci(split_data,
                          alpha = alpha,
                          sample_est = sample_est)
}


#' Percentile method
#' @param bootdist A vector containing the bootstrap distribution
#' @param alpha Alpha level to use for confidence interval calculation
#' @param sample_est The coefficient from a split procedure conducted in the original
#'   sample
#'
#' @rdname bootstrap_ci_methods
percentile_bootstrap_ci <- function(bootdist, alpha = 0.05, sample_est = NULL) {
  ql <- quantile(bootdist, c(alpha / 2, 1 - (alpha / 2)))
  citab <- data.table(method = "percentile", ci_alpha = alpha, ll = ql[1], ul = ql[2])
  if (!is.null(sample_est)) {
    citab[, estimate := sample_est]
    setcolorder(out, c("method", "estimate"))
  }
  citab
}

#' Bootstrap a split routine
#'
#' @param ... Pass arguments to [crosswalk()]
#' @param nboot Number of bootstrap replicates to generate
#' @param ncores Number of cores to use in parallel processing
#' @param seed Seed used by doRNG to generate reproducible parallel computations
#' @param alpha Alpha level for bootstrap confidence intervals
#'
#' @import data.table
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @export

bootstrap_crosswalk <- function(..., nboot, ncores = 1L, seed) {
  dcopy <- as.data.table(eval(match.call()$data))
  if (ncores == 1) {
    message("`ncores` is set to 1. Parallel processing will not be used.")
  }

  registerDoParallel(ncores)
  split_data <- foreach(i = seq_len(nboot),
                        .inorder = FALSE,
                        .combine = c,
                        .options.RNG = seed) %dorng% {
    datarep <- dcopy[sample(seq_len(.N), replace = TRUE)]
    cwargs <- list(...)
    cwargs$data <- datarep
    tmp <- do.call("crosswalk", cwargs)
    coef(tmp$fit)
  }
  stopImplicitCluster()

  split_data
}


#' Calculate bootstrapped confidence limits
#'
#' @param cx An object of class "cogxwalkr", i.e., an object returned by [crosswalk()].
#' @param alpha Alpha level to use for confidence interval calculation
#' @param type Type of confidence limits to calculate. Currently supported:
#'   percentile, normal.
#'
#' @rdname bootstrap_ci_methods
bootstrap_ci <- function(cx, alpha = 0.05, type = "percentile") {
  out <- list()
  out$alpha <- alpha
  out$se <- sd(cx$boot$dist)
  if ("normal" %in% type) {
    COEF <- unname(coef(cx$fit))
    ZQT <- qnorm(alpha / 2, lower.tail = FALSE)
    out$normal <- list(ll = COEF - ZQT * out$se,
                       ul = COEF + ZQT * out$se)
  }
  if ("percentile" %in% type || "perc" %in% type) {
    ql <- quantile(cx$boot$dist, c(alpha / 2, 1 - (alpha / 2)))
    out$percentile <- list(ll = unname(ql[1]), ul = unname(ql[2]))
  }
  out
}


#' Bootstrap control list
#'
#' @details Can be used as an argument to the `control` argument in [crosswalk()],
#'   but intended primarily to validate the list provided by that argument.
#' @param nboot Number of bootstrap replicates to produce.
#' @param seed Seed used to initialize parallel processing-safe random number
#'   generation.
#' @param ncores  Number of cores to use in parallel processing. Defaults to 1.
#'
#' @export
# TODO [2025-06-05] : add test
boot_control <- function(nboot = NULL, seed = NULL, ncores = 1L) {
  if (is.null(nboot) || !is.numeric(nboot)) {
    stop("`nboot` cannot be NULL and must be > 0")
  }
  if (is.null(seed) || !is.numeric(seed)) {
    stop("`seed` cannot be NULL")
  }
  list(nboot = nboot, seed = seed, ncores = ncores)
}

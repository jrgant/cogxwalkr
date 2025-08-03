#' Bootstrap a split routine
#'
#' @param ... Pass arguments to [crosswalk()]
#' @param nboot Number of bootstrap replicates to generate
#' @param ncores Number of cores to use in parallel processing. If set to 999,
#'   [parallelly::availableCores()] will use the maximum available
#' @param seed Seed used by doRNG to generate reproducible parallel computations
#'
#' @import data.table
#' @import foreach
#' @import future
#' @import doFuture
#' @importFrom parallelly availableCores
#' @export
bootstrap_crosswalk <- function(..., nboot, ncores = 1L, seed) {
  dcopy <- as.data.table(eval(match.call()$data))
  if (ncores == 1) {
    message("`ncores` is set to 1. Parallel processing will not be used.")
  }

  ## TODO: [2025-06-05] : Need to devise some tests for doFuture
  if (ncores < availableCores()) {
    message(sprintf("Running bootstraps over %d cores ...", ncores))
    plan(multisession, workers = ncores)
  } else {
    message(sprintf("Running bootstraps over %d cores ...", availableCores()))
    plan(multisession)
  }
  split_data <- foreach(
    i = seq_len(nboot),
    .inorder = FALSE,
    .combine = c,
    .options.future = list(seed = seed, packages = "cogxwalkr")
  ) %dofuture% {
    datarep <- dcopy[sample(seq_len(.N), replace = TRUE)]
    cwargs <- list(...)
    cwargs$data <- datarep
    tmp <- do.call("crosswalk", cwargs)
    coef(tmp$fit)[cwargs$cog1]
  }

  plan(sequential) # closes the workers opened by plan(multisession)

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
    COEF <- unname(coef(cx$fit)[cx$cog1])
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
boot_control <- function(nboot = NULL, seed = NULL, ncores = 1L) {
  if (is.null(nboot) || !is.numeric(nboot)) {
    stop("`nboot` cannot be NULL and must be a numeric value > 0")
  }
  if (is.null(seed) || !is.numeric(seed)) {
    stop("`seed` cannot be NULL and must be numeric")
  }
  if (is.null(ncores) || !is.numeric(ncores)) {
    stop("`ncores` cannot be NULL and must be a numeric value. To use a single core",
         " for processing, set `ncores = 1`.")
  }
  list(nboot = nboot, seed = seed, ncores = ncores)
}

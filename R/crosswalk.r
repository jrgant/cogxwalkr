#' Crosswalk cognitive measures
#'
#' @param cog1 The name of the first cognitive measure column
#' @param cog2 The name of the second cognitive measure column
#' @param data A data.table or data.frame containing the cognitive measure data
#' @param num_iter Number of split iterations to conduct
#' @param condition_by The name of a conditioning variable by which splits will be
#'   conducted. If not conducted, the function will use unconditional splits.
#' @param condition_loop Whether to conduct conditional splitting sequentially. Defaults
#'   to FALSE to maximize speed. Unused if `condition_by` is NULL. See documentation
#'   for `make_conditional_splits()` for details.
#' @param boot_control A list of settings passed to `bootstrap_crosswalk()` and functions
#'   that handle bootstrap confidence interval estimation. Also allows for some control
#'   of final output.
#'
#' @import data.table
#' @export
crosswalk <- function(cog1, cog2, data, num_iter = NULL,
                      condition_by = NULL, condition_loop = FALSE,
                      boot_control = list(...)) {

  if (is.data.frame(data) == FALSE || is.matrix(data)) {
    stop("The argument to `data` must be a data.frame, data.table, or matrix.")
  }

  tmp <- make_splits(cdvar = condition_by,
                     cdloop = condition_loop,
                     data = as.data.table(data),
                     num_iter = num_iter)

  ## calculate the mean difference in the cognitive measures by split
  diffs <- tmp[, .(
    cog1 = mean(get(cog1)[split_id == 1]) - mean(get(cog1)[split_id == 2]),
    cog2 = mean(get(cog2)[split_id == 1]) - mean(get(cog2)[split_id == 2])
  ), keyby = iteration]
  setnames(diffs, old = c("cog1", "cog2"), new = c(cog1, cog2))

  ## estimate correlation between the cognitive measure differences
  fml <- paste(cog2, "~", cog1, "- 1")
  fit <- lm(as.formula(fml), data = diffs)

  ## store model fit and mean differences
  out <- list(fit = fit, diffs = diffs)

  if (!missing("boot_control")) {
    arglist <- as.list(match.call())[-1]
    control <- as.list(arglist[["boot_control"]])[-1]
    control[["alpha"]] <- NULL
    # drop boot_control so that we don't re-enter this loop during the bootstraps
    argsboot <- c(arglist[-which(names(arglist) %in% "boot_control")], control)
    out[["boot"]][["fits"]] <- do.call("bootstrap_crosswalk", args = argsboot)
    # calculate stats
    ALPHA <- ifelse(!is.null(boot_control$alpha), boot_control$alpha, 0.05)
    out[["boot"]] <- percentile_bootstrap_ci(out[["boot"]][["fits"]], ALPHA)
    # save bootstrap models or not
    if (is.null(boot_control$keep_fits) || boot_control$keep_fits == FALSE) {
      out[["boot"]][["fits"]] <- NULL
    }
  }

  out
}

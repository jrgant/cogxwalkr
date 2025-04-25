#' Generate data splits
#'
#' @param cog1 The name of the first cognitive measure column
#' @param cog2 The name of the second cognitive measure column
#' @param data A data.table or data.frame containing the cognitive measure data
#' @param num_iter Number of split iterations to conduct
#' @param condition_by The name of a conditioning variable by which splits will be
#'   conducted. If not conducted, the function will use unconditional splits.
#' @param boot_ci Boolean indicating whether to generate bootstrap confidence intervals
#' @param boot_control A list of settings passed to `bootstrap_crosswalk()`
#'
#' @import data.table
#' @export
crosswalk <- function(cog1, cog2, data, num_iter,
                      condition_by = NULL,
                      boot_ci = FALSE, boot_control = list(...)) {

  if (boot_ci == TRUE && missing("boot_control")) {
    stop("When requesting bootstrap confidence intervals, `boot_control` requires ",
         "a list setting the following arguments at a minimum: num_boot, rng_seed. ",
         "We also recommend setting num_cores to enable parallel processing.")
  }

  tmp <- make_splits(split_var = condition_by, data = data, num_iter = num_iter)

  ## calculate the mean difference in the cognitive measures by split
  diffs <- tmp[, .(
    cog1 = mean(get(cog1)[split == 1]) - mean(get(cog1)[split == 2]),
    cog2 = mean(get(cog2)[split == 1]) - mean(get(cog2)[split == 2])
  ), keyby = iteration]
  setnames(diffs, old = c("cog1", "cog2"), new = c(cog1, cog2))

  ## estimate correlation between the cognitive measure differences
  fml <- paste(cog2, "~", cog1, "- 1")
  fit <- lm(as.formula(fml), data = diffs)

  ## store model fit and mean differences
  out <- list(fit = fit, diffs = diffs)

  if (boot_ci == TRUE) {
    arglist <- as.list(match.call())[-1]
    # force boot_ci=FALSE b/c crosswalk() is called by the bootstrap function
    arglist[["boot_ci"]] <- FALSE
    control <- as.list(arglist[["boot_control"]])[-1]
    arglist <- c(arglist, control)
    out[["ci"]] <- do.call("bootstrap_crosswalk", args = arglist)
  }

  out
}

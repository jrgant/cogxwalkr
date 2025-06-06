#' Crosswalk cognitive measures
#'
#' @param cog1 The name of the first cognitive measure column
#' @param cog2 The name of the second cognitive measure column
#' @param data A data.table, data.frame, matrix, or list containing the
#'   cognitive measure data
#' @param niter Number of iterations to conduct for an unconditional split routine
#' @param condition_by The name of a conditioning variable by which splits will be
#'   conducted. If not conducted, the function will use unconditional splits.
#' @param condition_loop Whether to conduct conditional splitting sequentially. Defaults
#'   to FALSE to maximize speed. Unused if `condition_by` is NULL. See documentation
#'   for [make_conditional_splits()] for details.
#' @param control A list of settings passed to [bootstrap_crosswalk()]. See
#'   [boot_control()] for more information.
#'
#' @import data.table
#' @export
crosswalk <- function(cog1, cog2, data, niter = NULL,
                      condition_by = NULL, condition_loop = FALSE,
                      control = list(...)) {

  if (is.data.frame(data) == FALSE || is.matrix(data) || is.list(data) == FALSE) {
    stop("The argument to `data` must be a data.frame, data.table, or matrix.")
  }

  tmp <- make_splits(cdvar = condition_by,
                     cdloop = condition_loop,
                     data = as.data.table(data),
                     niter = niter)

  ## calculate the mean difference in the cognitive measures by split
  diffs <- tmp[, .(
    cog1 = mean(get(cog1)[split_id == 1]) - mean(get(cog1)[split_id == 2]),
    cog2 = mean(get(cog2)[split_id == 1]) - mean(get(cog2)[split_id == 2])
  ), keyby = iteration]
  setnames(diffs, old = c("cog1", "cog2"), new = c(cog1, cog2))

  ## estimate correlation between the cognitive measure differences
  fit <- diffs[, lm(cog2 ~ cog1 - 1), env = list(cog1 = cog1, cog2 = cog2)]
  condition_var <- ifelse(!is.null(condition_by), condition_by, "")

  ## store model fit and mean differences
  out <- list(fit = fit, diffs = diffs, condition_var = condition_var)

  if (!missing("control")) {
    arglist <- as.list(match.call())[-1]
    control <- do.call("boot_control", control)
    # drop control so that we don't re-enter this loop during the bootstraps
    argsboot <- c(arglist[-which(names(arglist) %in% "control")], control)
    out[["boot"]][["dist"]] <- unname(do.call("bootstrap_crosswalk", args = argsboot))
    attr(out[["boot"]][["dist"]], "coef") <- cog1
  }

  class(out) <- c("cogxwalkr", "list")
  out
}


#' Summarize a cogxwalkr list
#'
#' @param cx An object of class "cogxwalkr", i.e., an object returned by [crosswalk()].
#' @param alpha Alpha to use for confidence interval calculation. Defaults to 0.05.
#' @param bci_type Type of bootstrapped confidence interval to calculate. Currently
#'   accepts "percentile" (or "perc") and/or "normal." Both are included by default.
#'
#' @export
summary.cogxwalkr <- function(cx, alpha = 0.05, bci_type = c("percentile", "normal")) {
  out <- list(
    fml = deparse(cx$fit$call$formula),
    sample_est = unname(coef(cx$fit)),
    condition_var = ifelse(cx$condition_var == "", "none", cx$condition_var),
    niter = nrow(cx$diffs)
  )
  if (!is.null(cx$boot)) {
    out$nboot <- length(cx$boot$dist)
    out$boot_est  <-  mean(cx$boot$dist)
    out$ci <- bootstrap_ci(cx, alpha, type = bci_type)
  }
  class(out) <- c("summary.cogxwalkr", "list")
  out
}


#' Print a cogxwalkr summary
#'
#' @param digits Number of digits to print. Passed to [base::round()].
#'
#' @export
print.summary.cogxwalkr <- function(x, digits = 3L) {
  fd <- function(num) round(num, digits = digits)
  indent <- paste(rep(" ", 2), collapse = "")
  hr <- paste0("\n", paste(rep("-", 50), collapse = ""), "\n")

  cat(hr,
      "Crosswalk Summary", hr,
      "Formula:         ", x$fml, "\n",
      "Coefficient:     ", fd(x$sample_est), "\n\n",
      paste0((1 - x$ci$alpha) * 100, "% confidence limits:"), "\n",
      sep = "")

  ci_types <- names(x$ci)[!names(x$ci) %in% c("alpha", "se")]
  sapply(ci_types, \(.x) {
    tmp <- x[["ci"]][[.x]]
    cat(indent, paste0("(", fd(tmp$ll), ", ", fd(tmp$ul), ")"), " - ", .x, "\n", sep = "")
  })
  cat("\n")
  cat(indent, "Based on ", x$nboot, " bootstrap replicates\n",
      indent, "SE = ", fd(x$ci$se),
      sep = "")

  cat(hr,
      "Number of iterations: ", x$niter, "\n",
      "Conditioning variable: ", x$condition_var, "\n\n",
      sep = "")
}


#' Plot information about the bootstrap distribution
#'
#' @param cxsum The output of `summary(cx)`
#' @param types The types of crosswalk plots to produce. By default, both a plot of the
#'   bootstrap distribution of coefficients and a plot of the data with the estimated
#'   slope. For the plotted sloped, (1-alpha)% confidence intervals will appear if the
#'   user provides the output of [summary.cogxwalkr()].
#' @param breaks Passed to [graphics::hist()], overriding the default method with "FD"
#'   ([grDevices::nclass.FD()]).
#' @param citype Choose confidence intervals to plot. Ignored if `cxsum` is NULL.
#' @param layout Passed to the `mfrow` argument of [graphics::par()]. Defaults to
#'   `c(1, length(types))`.
#' @param sarg List of parameters passed to the [graphics::abline()] that plots the sample
#'   coefficient estimate
#' @param barg List of parameters passed to the [graphics::abline()] that plots the mean
#'   coefficient estimate across bootstrap replicates
#' @inheritParams summary.cogxwalkr
#'
#' @import data.table
#' @export
plot.cogxwalkr <- function(cx, cxsum = NULL, types = c("boot", "slope"),
                           breaks = "FD", citype = "percentile",
                           layout = c(1, length(types)),
                           sarg = list(col = "black", lty = 1, lwd = 2),
                           barg = list(col = "red", lty = 2, lwd = 2)) {
  COEF <- coef(cx$fit)

  par(mfrow = layout)
  if ("boot" %in% types) {
    ## TODO: [2025-06-05] : write test
    if (is.null(cx$boot)) {
      stop("No bootstrap data provided. To see only the scatterplot of differences and",
           "the slope estimated in the sample, set `types` to 'slope'.")
    }
    hist(cx$boot$dist, breaks = breaks,
         xlab = sprintf("Bootstrap distribution of the %s coefficient", names(COEF)),
         main = sprintf("R = %d", length(cx$boot$dist)))
    do.call("abline", args = c(list(v = COEF), sarg))
    do.call("abline", args = c(list(v = mean(cx$boot$dist)), barg))
    legend("topright",
           legend = c("Sample coef.", "Bootstrap coef."),
           col = c(sarg$col, barg$col),
           lty = c(sarg$lty, barg$lty),
           lwd = c(sarg$lwd, barg$lwd),
           bty = "n")
  }

  if ("slope" %in% types) {
    fdat <- as.data.table(cx$fit$model)
    plot(fdat$mmse, fdat$moca, pch = 19, col = scales::alpha("black", 0.2),
         main = "Crosswalk")
    abline(0, COEF, col = "red", lwd = 2)
    ## TODO: [2025-06-06] : write test for detection of cxsum and citype
    if (!is.null(cxsum)) {
      if (length(citype) > 1) {
        stop("length of `citype` must be 1")
      }
      abline(0, cxsum$ci[[citype]]$ll, col = "red", lty = 2)
      abline(0, cxsum$ci[[citype]]$ul, col = "red", lty = 2)
    }
  }

}

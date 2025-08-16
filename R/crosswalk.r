#' Estimate crosswalk between cognitive measures
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
#' @examples
#' # linear model estimate of slope (no splitting)
#' crosswalk(cog1 = "mmse", cog2 = "moca", data = cogsim) # linear model
#'
#' # unconditional split method
#' crosswalk(cog1 = "mmse", cog2 = "moca", data = cogsim, niter = 500)
#'
#' # conditional split method
#' crosswalk(cog1 = "mmse", cog2 = "moca", data = cogsim, condition_by = "dementia")
#'
#' # regardless of the method, request bootstrap via `control`
#' crosswalk(cog1 = "mmse", cog2 = "moca", data = cogsim,
#'           control = list(nboot = 1000, seed = 999, ncores = 4))
crosswalk <- function(cog1, cog2, data, niter = NULL,
                      condition_by = NULL, condition_loop = FALSE, control = NULL) {

  ## Use the unconditional iteration method if `niter` is provided
  if (!is.null(niter) || !is.null(condition_by)) {

    tmp <- make_splits(cdvar = condition_by,
                       cdloop = condition_loop,
                       data = data,
                       niter = niter)

    ## calculate the mean difference in the cognitive measures by split
    diffs <- tmp[, list(cog1 = mean(m1[split_id == 1]) - mean(m1[split_id == 2]),
                        cog2 = mean(m2[split_id == 1]) - mean(m2[split_id == 2])),
                 keyby = iteration,
                 env = list(m1 = cog1, m2 = cog2)]
    setnames(diffs, old = c("cog1", "cog2"), new = c(cog1, cog2))

    ##  estimate correlation between the cognitive measure differences
    fit <- diffs[, lm(cog2 ~ cog1 - 1), env = list(cog1 = cog1, cog2 = cog2)]

  } else if (is.null(niter) && is.null(condition_by)) {

    data_dt <- ingest_data(data)

    fit <- data_dt[, lm(cog2 ~ cog1), env = list(cog1 = cog1, cog2 = cog2)]
    diffs <- NULL

  }

  ## store model fit and mean differences
  out <- list(cog1 = cog1,
              cog2 = cog2,
              fit = fit,
              diffs = diffs,
              condition_var = condition_by)

  if (!is.null(control)) {
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

# Avoid R CMD check notes related to non-standard evaluation in data.table
utils::globalVariables(c("m1", "m2", "split_id", "iteration"))


#' Summarize a cogxwalkr list
#'
#' @param object An object of class "cogxwalkr", i.e., as returned by [crosswalk()].
#' @param ... Unused
#' @param alpha Alpha to use for confidence interval calculation. Defaults to 0.05.
#' @param bci_type Type of bootstrapped confidence interval to calculate. Currently
#'   accepts "percentile" (or "perc") and/or "normal." Both are included by default.
#'
#' @export
summary.cogxwalkr <- function(object, ...,
                              alpha = 0.05, bci_type = c("percentile", "normal")) {
  out <- list(
    fml = deparse(object$fit$call$formula),
    sample_est = unname(coef(object$fit)[length(coef(object$fit))]),
    condition_var = ifelse(object$condition_var == "", "none", object$condition_var),
    niter = nrow(object$diffs)
  )
  if (!is.null(object$boot)) {
    out$nboot <- length(object$boot$dist)
    out$boot_est  <-  mean(object$boot$dist)
    out$ci <- bootstrap_ci(object, alpha, type = bci_type)
  }
  class(out) <- c("summary.cogxwalkr", "list")
  out
}

#' Print a cogxwalkr summary
#'
#' @param x An object of class "summary.cogxwalkr", i.e., as returned by
#'   [summary.cogxwalkr()].
#' @param ... Unused
#' @param digits Number of digits to print. Passed to the `digits` and `nsmall` arguments
#'   to [base::format()].
#' @export
print.summary.cogxwalkr <- function(x, ..., digits = 3L) {
  fd <- function(num) format(num, digits = digits, nsmall = digits)
  indent <- paste(rep(" ", 2), collapse = "")
  hr <- paste0("\n", paste(rep("-", 50), collapse = ""), "\n")

  cat(hr,
      "Crosswalk Summary (Adjunct)", hr,
      "Formula:         ", x$fml, "\n",
      "Coefficient:     ", fd(x$sample_est), "\n\n",
      sep = "")

  if (!is.null(x$ci)) {
    cat(paste0((1 - x$ci$alpha) * 100, "% confidence limits:\n"))
    ci_types <- names(x$ci)[!names(x$ci) %in% c("alpha", "se")]
    sapply(ci_types, \(.x) {
      tmp <- x[["ci"]][[.x]]
      cat(indent, paste0("(", fd(tmp$ll), ", ", fd(tmp$ul), ")"), " - ", .x, "\n",
          sep = "")
    })
    cat("\n")
    cat(indent, "Based on ", x$nboot, " bootstrap replicates\n",
        indent, "SE = ", fd(x$ci$se),
        sep = "")
  }
  cat(hr,
      "Number of iterations: ", x$niter, "\n",
      "Conditioning variable: ", x$condition_var, "\n\n",
      sep = "")
}


#' Plot information about the bootstrap distribution
#'
#' @param x An object of class "cogxwalkr", i.e., as returned by [crosswalk()].
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
#' @param sargs List of parameters passed to the [graphics::abline()] that plots the
#'   sample coefficient estimate
#' @param bargs List of parameters passed to the [graphics::abline()] that plots the mean
#'   coefficient estimate across bootstrap replicates
#' @param slargs List of parameters passed to the [graphics::abline()] that plots the
#'   sample slope in the crosswalk panel
#' @param clargs List of parameters passed to the [graphics::abline()] calls that plot
#'   the confidence limits for the slope.
#' @param ptsize Size of points in crosswalk scatterplot (passed to [base::plot()])
#' @param ptshape Shape of points in crosswalk scatterplot (passed to [base::plot()])
#' @param ptcol Color of points in crosswalk scatterplot (passed to [base::plot()])
#' @param ptalpha Alpha (transparency) of points in crosswalk scatterplot
#'   (passed to [base::plot()])
#'
#' @inheritParams print.summary.cogxwalkr
#'
#' @import data.table
#' @import stats
#' @import graphics
#' @importFrom scales alpha
#' @importFrom grDevices dev.off
#' @export
plot.cogxwalkr <- function(x, ...,
                           cxsum = NULL, types = c("boot", "slope"),
                           breaks = "FD", citype = "percentile",
                           layout = c(1, length(types)),
                           sargs = list(col = "black", lty = 1, lwd = 2),
                           bargs = list(col = "red", lty = 2, lwd = 2),
                           slargs = list(col = "red", lwd = 2),
                           clargs = list(col = "red", lty = 2),
                           ptshape = 19, ptsize = 0.8, ptcol = "black", ptalpha = 0.2) {
  COEF <- coef(x$fit)
  COEF <- COEF[names(COEF) != "(Intercept)"]

  if (!all(types %in% c("boot", "slope"))) {
    stop("invalid types argument provided")
  }

  if ("slope" %in% types && is.null(x$diffs)) {
    message("The crosswalk() object does not contain differences, most likely because ",
            "the slope was calculated using the manual method and not unconditional ",
            "splits. The scatterplot has been omitted.")
    par(mfrow = c(1, 1))
  } else {
    par(mfrow = layout)
  }

  if ("boot" %in% types) {
    if (is.null(x$boot) && !is.null(x$diffs)) {
      dev.off()
      stop("No bootstrap data provided. To see only the scatterplot of differences and ",
           "the slope estimated in the sample, set `types` to 'slope'.")
    }
    if (is.null(x$boot) && is.null(x$diffs)) {
      dev.off()
      stop("No bootstrap data provided.")
    }
    hist(x$boot$dist, breaks = breaks,
         xlab = sprintf("Bootstrap distribution of the %s coefficient", names(COEF)),
         main = sprintf("R = %d", length(x$boot$dist)))
    do.call("abline", args = c(list(v = COEF), sargs))
    do.call("abline", args = c(list(v = mean(x$boot$dist)), bargs))
    legend("topright",
           legend = c("Sample coef.", "Bootstrap coef."),
           col = c(sargs$col, bargs$col),
           lty = c(sargs$lty, bargs$lty),
           lwd = c(sargs$lwd, bargs$lwd),
           bty = "n")
  }

  if ("slope" %in% types && !is.null(x$diffs)) {
    fdat <- as.data.table(x$fit$model)
    plot(fdat[[x$cog2]] ~ fdat[[x$cog1]],
         cex = ptsize,
         pch = ptshape,
         col = scales::alpha(ptcol, ptalpha),
         main = deparse(x$fit$call$formula),
         xlab = x$cog1,
         ylab = x$cog2)
    do.call("abline", args = c(list(a = 0, b = COEF), slargs))

    if (!is.null(cxsum)) {
      if (length(citype) > 1) {
        dev.off()
        stop("length of `citype` must be 1")
      }
      do.call("abline", args = c(list(a = 0, b = cxsum$ci[[citype]]$ll), clargs))
      do.call("abline", args = c(list(a = 0, b = cxsum$ci[[citype]]$ul), clargs))
    }
  }
}


#' @details Unlike [crosswalk()], which uses splitting to estimate the sample coefficient,
#'   [est_cw_coef()] calculates the coefficient based on summary statistics from the input
#'   data: cov(cog1, cog2) / var(cog1).
#'
#' @param method Either 'lm' (recommended) or 'manual'. The former will fit a linear
#'   regression model and return the fit object, while the latter will return a table
#'   containing the covariance between the cognitive measures (`cov`), the variance of
#'   the measure input as cog1 (`var`), and the estimated sloped (`coef`).
#'
#' @import data.table
#' @rdname crosswalk
#' @export
est_cw_coef <- function(cog1, cog2, data, method = "lm") {
  if (method == "lm") {
    out <- data[, lm(m2 ~ m1, data), env = list(m1 = cog1, m2 = cog2)]
  } else if (method == "manual") {
    out <- data[, list(
      cov = cov(m1, m2),
      var = var(m1),
      coef = cov(m1, m2) / var(m1)
    ), env = list(m1 = cog1, m2 = cog2)]
  } else {
    stop("`method` must be one of 'lm' or 'manual'")
  }
  out
}


#' @title Crosswalk an effect estimate
#'
#' @description
#' Take a published effect estimate (e.g., the difference in Mini Mental State
#' Exam score comparing APOE-ε4 carriers to non-carriers) and translate that effect
#' estimate to an alternate scale (e.g., Montreal Cognitive Assessment). The translation
#' uses a crosswalk estimated via [crosswalk()] in data where both measures are available.
#'
#' @param object An object of class `cogxwalkr` or the result of [est_cw_coef()]
#' @param est_mean Point estimate (beta) to be crosswalked to the alternative outcome
#'   measure
#' @param est_se The standard error of `est_mean`
#' @param est_ci The lower (1-alpha)% confidence interval of `est_mean`
#' @param est_pval The p-value corresponding to `est_mean`
#' @param est_indep The independent variable to which `est_mean` applies
#' @param est_outcome The outcome measure in the original study (e.g., "MOCA", "MMSE")
#' @param est_alpha The alpha level for the confidence interval (if `est_se` is provided)
#'   or the alpha level that will be used to back-calculate the standard error from
#'   `est_ci`. Defaults to 0.05.
#' @param alpha The alpha level for the confidence interval of the crosswalked estimate.
#'   Defaults to 0.05.
#'
#' @details
#' Parameters prefixed with `est_` refer to a summary estimate for which the user lacks
#' access to the underlying data but wishes to translate the estimate to another
#' cognitive measure's scale. The user must supply `est_mean` and one of `est_se`,
#' `est_ci`, or `est_pval`. [do_crosswalk()] will back-calculate the standard error if
#' necessary, as follows:
#'   - `est_ci` : `(confidence interval width) / 2 / (critical value)`, where "critical
#'      value" refers to the Z-value of the standard normal distribution assuming a
#'      two-sided `est_alpha`
#'   - `est_pval` : `est_mean / (critical value)`, where "critical value" in this case
#'     is calculated assuming a two-sided p-value
#'
#' As in the [Cochrane Handbook](https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-06#section-6-3-1) summary of these calculations, the function assumes that statistical estimates
#' for difference measures were calculated using the standard normal distribution rather
#' than a t-distribution.
#' @export
do_crosswalk <- function(object,
                         est_mean = NULL, est_se = NULL, est_ci = NULL,
                         est_pval = NULL, est_alpha = 0.05,
                         est_indep = NULL, est_outcome = NULL,
                         alpha = 0.05) {

  # Helper function to get the critical value based on alpha
  get_crit <- function(x) qnorm(x / 2, lower.tail = FALSE)

  # Slope from a crosswalk estimation
  coefs <- summary(object$fit)$coefficients
  SLOPE <- coefs[dim(coefs)[1], "Estimate"]
  SLOPE_SE <- coefs[dim(coefs)[1], "Std. Error"]

  ## TODO: [2025-07-27] : add tests
  if (is.null(est_mean)) {
    stop("`est_mean` cannot be NULL")
  } else {
    EST_MEAN <- unname(est_mean)
  }
  # Retrieve or calculate standard error from study estimate
  if (!is.null(est_se)) {

    if (!is.null(est_ci)) {
      warning("both `est_se` and `est_ci` provided... ignoring `est_ci`.")
    }
    EST_SE <- est_se

  } else if (!is.null(est_ci)) {

    if (length(est_ci) != 2) {
      stop("length of `est_ci` must be 2")
    }
    EST_SE <- as.vector(dist(est_ci, method = "euclidean")) / 2 / get_crit(est_alpha)

  } else if (!is.null(est_pval)) {

    # https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-06
    # Section 6.3.1
    EST_SE <- est_mean / qnorm(est_pval / 2, lower.tail = FALSE)

  } else {
    stop("must provide `est_se`, `est_ci`, or `est_pval`")
  }

  # Crosswalked estimate
  CW_EST <- SLOPE * EST_MEAN
  CW_SE <- sqrt(CW_EST^2 * ((SLOPE_SE / SLOPE)^2 + (EST_SE / EST_MEAN)^2))
  CW_CI <- sapply(
    c(`-`, `+`),
    \(f) f(CW_EST, get_crit(alpha) * CW_SE)
  )

  out <- list()
  out$estimate <- list(mean = EST_MEAN,
                       se = EST_SE,
                       alpha = est_alpha,
                       outcome = est_outcome,
                       predictor = est_indep)
  out$cxest <- list(slope = SLOPE,
                    se = SLOPE_SE,
                    model = deparse(object$fit$call$formula))
  out$crosswalk <- list(mean = CW_EST,
                        se = CW_SE,
                        ll = CW_CI[which.min(CW_CI)],
                        ul = CW_CI[which.max(CW_CI)],
                        alpha = alpha)

  class(out) <- c("cogxwalkr.crosswalk", "list")
  out
}

#' Print the results of a crosswalk
#'
#' @param x An object of class "cogxwalkr.crosswalk", i.e., as returned by
#'   [do_crosswalk()]
#' @inheritParams print.summary.cogxwalkr
#' @export
print.cogxwalkr.crosswalk <- function(x, ..., digits = 3L) {
  fd <- function(num) format(num, digits = digits, nsmall = digits)
  hr <- paste0("\n", paste(rep("-", 50), collapse = ""), "\n")
  CLLAB <- (1 - x$crosswalk$alpha) * 100
  cat(hr,
      "Crosswalk Summary",
      hr,
      "Adjunct Estimate:\n",
      "    ", fd(x$cxest$slope), ", SE: ", fd(x$cxest$se), "\n",
      "    ", x$cxest$model, "\n\n",
      "Study Estimate:\n",
      "    ", fd(x$estimate$mean), ", SE: ", fd(x$estimate$se), "\n",
      "    ", paste(x$estimate$outcome, "~", x$estimate$predictor), "\n\n",
      "Crosswalked Estimate:\n",
      "    ", fd(x$crosswalk$mean), ", SE: ", fd(x$crosswalk$se), "\n",
      "    ", CLLAB, "% confidence limits: (",
      fd(x$crosswalk$ll), ", ", fd(x$crosswalk$ul), ")",
      hr, sep = "")
}

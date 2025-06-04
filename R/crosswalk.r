#' Crosswalk cognitive measures
#'
#' @param cog1 The name of the first cognitive measure column
#' @param cog2 The name of the second cognitive measure column
#' @param data A data.table, data.frame, matrix, or list containing the
#'   cognitive measure data
#' @param niter Number of split iterations to conduct
#' @param condition_by The name of a conditioning variable by which splits will be
#'   conducted. If not conducted, the function will use unconditional splits.
#' @param condition_loop Whether to conduct conditional splitting sequentially. Defaults
#'   to FALSE to maximize speed. Unused if `condition_by` is NULL. See documentation
#'   for `make_conditional_splits()` for details.
#' @param control A list of settings passed to `bootstrap_crosswalk()`. See
#'   `boot_control()` for more information.
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
  fml <- paste(cog2, "~", cog1, "- 1")
  fit <- lm(as.formula(fml), data = diffs)
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
#' @param cx An object of class "cogxwalkr", i.e., an object returned by `crosswalk()`.
#' @param alpha Alpha to use for confidence interval calculation. Defaults to 0.05.
#' @param bci_type Type of bootstrapped confidence interval to calculate. Currently
#'   accepts "percentile" (or "perc") and/or "normal." Both are included by default.
#'
#' @export
summary.cogxwalkr <- function(cx, alpha = 0.05, bci_type = c("percentile", "normal")) {
  out <- list(
    fml = paste(c(terms(cx$fit)[[2]], "~", terms(cx$fit)[[3]]), collapse = " "),
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
#' @param digits Number of digits to print. Passed to `round()`.
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

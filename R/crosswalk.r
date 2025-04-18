#' Generate data splits
#'
#' @param cog1 The name of the first cognitive measure column
#' @param cog2 The name of the second cognitive measure column
#' @param data A data.table or data.frame containing the cognitive measure data
#' @param num_iter Number of split iterations to conduct
#' @param type One of "unconditional" or "conditional". A conditional split will conduct
#'   the splits independently within levels of an auxiliary variable (see `condition_by`).
#' @param condition_by The name of a conditioning variable by which splits will be
#'   conducted when type="conditional"
#'
#' @import data.table
#' @export
crosswalk <- function(cog1, cog2, data, num_iter,
                      type = "unconditional", condition_by = NULL,
                      boot_ci = FALSE) {

  if (type == "unconditional" && !is.null(condition_by)) {
    warning(
      "Unconditional split requested, but a conditioning variable is indicated in ",
      "`condition_by`."
    )
  }

  N_INPUT <- nrow(data)
  SPLIT_POINT <- floor(N_INPUT / 2)

  ## create a dataset of num_iter stacked replicates
  tmp <- data[sample(seq_len(N_INPUT), size = N_INPUT * num_iter, replace = TRUE)]

  tmp[, `:=`(
    split = rep(
      c(rep(1, SPLIT_POINT), rep(2, N_INPUT - SPLIT_POINT)),
      times = num_iter
    ),
    iteration = rep(seq_len(num_iter), each = N_INPUT)
  )]
  tmp[]

  ## calculate the mean difference in the cognitive measures by split
  diffs <- tmp[, .(
    cog1 = mean(get(cog1)[split == 1]) - mean(get(cog2)[split == 2]),
    cog2 = mean(get(cog2)[split == 1]) - mean(get(cog2)[split == 2])
  ), keyby = iteration]
  setnames(diffs, old = c("cog1", "cog2"), new = c(cog1, cog2))

  ## estimate correlation between the cognitive measure differences
  fml <- paste(cog2, "~", cog1, "- 1")
  fit <- lm(as.formula(fml), data = diffs)

  out <- list(fit = fit, diffs = diffs)
  out
}

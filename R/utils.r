#' Make an unconditional split dataset
#'
#' @param num_iter Number of split iterations to conduct
#' @param data Input dataset

#' @noRd
make_unconditional_splits <- function(data, num_iter) {
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
}

#' Make a split dataset
#' @param split_var Auxiliary variable by which to stratify splits
#'
#' @inheritParams make_unconditional_split
#' @import foreach
#' @noRd
make_splits <- function(split_var = NULL, data, num_iter) {
  if (is.null(split_var)) {
    tmpout <- make_unconditional_splits(data = data, num_iter = num_iter)
  } else {
    tmpc <- copy(data)
    CLEVELS <- unique(tmpc[, get(split_var)])
    tmpout <- foreach(i = CLEVELS, .combine = rbind) %do% {
      make_unconditional_splits(data = tmpc[get(split_var) == i], num_iter = num_iter)
    }
  }
  tmpout
}

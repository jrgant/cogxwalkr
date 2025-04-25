#' Make an unconditional split dataset
#'
#' @param num_iter Number of split iterations to conduct
#' @param data Input dataset
#'
#' @noRd
make_unconditional_splits <- function(data, num_iter) {
  N_INPUT <- nrow(data)
  SPLIT_POINT <- floor(N_INPUT / 2)

  ## create a dataset of num_iter stacked replicates
  tmp <- data[sample(seq_len(N_INPUT), size = N_INPUT * num_iter, replace = TRUE)]

  tmp[, `:=`(
    split_id = rep(
      c(rep(1, SPLIT_POINT), rep(2, N_INPUT - SPLIT_POINT)),
      times = num_iter
    ),
    iteration = rep(seq_len(num_iter), each = N_INPUT)
  )]
  tmp[]
}

#' Make a conditional split dataset
#'
#' @param data Input dataset
#' @param cdvar Character string naming auxiliary variable by which to condition splits
#'
#' @noRd
make_conditional_splits <- function(cdvar = NULL, data) {
  if (is.null(cdvar)) {
    stop("To conduct conditional splitting, a conditioning variable must be specified.")
  }

  CLEVELS <- sort(unique(data[, get(cdvar)]), decreasing = TRUE)
  if (length(CLEVELS) != 2) {
    stop("Conditioning variable must be binary.") ## TODO add test
  }

  NUM_ITER <- data[, sum(get(cdvar))]
  spec <- data.table(num = c(seq(NUM_ITER, 1), seq(1, NUM_ITER)),
                     level = c(rep(CLEVELS[1], NUM_ITER), rep(CLEVELS[2], NUM_ITER)))


}

#' Make a split dataset
# '
#' @param cdvar Character string naming auxiliary variable by which to condition splits
#'
#' @inheritParams make_unconditional_split
#' @import foreach
#' @noRd
make_splits <- function(cdvar = NULL, data, num_iter) {
  if (is.null(cdvar)) {
    tmpout <- make_unconditional_splits(data = data, num_iter = num_iter)
  } else {
    tmpc <- copy(data)
    CLEVELS <- unique(tmpc[, get(cdvar)])
    ## BUG This should be implemented so that in a dataset of 1000 people:
    ##    1) Split 1: we go from 1 dementia / 999 no dementia in the first iteration
    ##       to 999 dementia / 1 no dementia in the last iteration
    ##    2) Split 2 is the reverse
    ##    3) N = number of rows in the base dataset
    tmpout <- foreach(i = CLEVELS, .combine = rbind) %do% {
      make_unconditional_splits(data = tmpc[get(cdvar) == i], num_iter = num_iter)}
  }
  tmpout
}

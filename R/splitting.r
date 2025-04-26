#' Make an unconditional split dataset
#'
#' @param num_iter Number of split iterations to conduct
#' @param data Input dataset
#'
#' @noRd
make_unconditional_splits <- function(data, num_iter) {
  ## TODO: [2026-04-26] : add test
  if (is.null(num_iter)) {
    stop("The parameter `num_iter` must be set in order to do unconditional splits.")
  }

  N_INPUT <- nrow(data)
  SPLIT_POINT <- floor(N_INPUT / 2)

  ## create a dataset of num_iter stacked replicates
  rowlist <- unlist(replicate(num_iter, sample(seq_len(N_INPUT)), simplify = FALSE))
  tmp <- data[rowlist]

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

  ## TODO: [2025-04-25] : add test
  CLEVELS <- sort(unique(data[, get(cdvar)]))
  if (length(CLEVELS) != 2) {
    stop("Conditioning variable must be binary.")
  }

  ## TODO: [2025-04-25] : add test for spec formatting
  NUM_DATA <- nrow(data)
  SPLIT1_SIZE <- floor(NUM_DATA / 2)
  SPLIT2_SIZE <- NUM_DATA - SPLIT1_SIZE
  L1_SIZES <- seq_len(data[, sum(get(cdvar) == CLEVELS[2]) - 1])

  spec <- data.table(sl_11_size = L1_SIZES)
  spec[, sl_10_size := SPLIT1_SIZE - sl_11_size]

  ## TODO: [2025-04-26] : speed up, will take awhile w/ bootstrapping
  ## sample rows from original data according to spec
  tmp <- foreach(i = seq_len(nrow(spec)), .combine = rbind) %do% {
    shuffle <- spec[i, {
      lrows1 <- data[get(cdvar) == CLEVELS[2]][sample(seq_len(.N))]
      lrows1[, split_id := rep(c(1, 2), times = c(sl_11_size, .N - sl_11_size))]
      lrows0 <- data[get(cdvar) == CLEVELS[1]][sample(seq_len(.N))]
      lrows0[, split_id := rep(c(1, 2), times = c(sl_10_size, .N - sl_10_size))]
      rbind(lrows1, lrows0)[, iteration := i][]
    }]
    shuffle
  }

  tmp[]
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
    ## TODO: [2025-04-26] : add test for this message
    if (!is.null(num_iter)) {
      message("Ignoring `num_iter`, because the number of iterations will be determined ",
              "by the number of rows in the second level of the binary conditioning ",
              "variable.")
    }
    tmpout <- make_conditional_splits(cdvar = cdvar, data = data)
  }
  tmpout
}

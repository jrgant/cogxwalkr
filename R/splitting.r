#' Make an unconditional split dataset
#'
#' @param niter Number of split iterations to conduct
#' @param data Input dataset
#'
#' @rdname splitting_functions
#' @import data.table
#' @export
make_unconditional_splits <- function(data, niter) {
  if (is.null(niter)) {
    stop("The parameter `niter` must be set in order to do unconditional splits.")
  }

  data <- as.data.table(data)
  N_INPUT <- nrow(data)
  SPLIT_POINT <- floor(N_INPUT / 2)

  ## create a dataset of niter stacked replicates
  rowlist <- unlist(replicate(niter, sample(seq_len(N_INPUT)), simplify = FALSE))
  tmp <- data[rowlist]

  tmp[, `:=`(
    split_id = rep(
      c(rep(1, SPLIT_POINT), rep(2, N_INPUT - SPLIT_POINT)),
      times = niter
    ),
    iteration = rep(seq_len(niter), each = N_INPUT)
  )]
  tmp[]
}

#' Make a conditional split dataset
#'
#' @param cdvar Character string naming auxiliary variable by which to condition splits
#' @param data Input dataset
#' @param loop Boolean declaring whether to use a for loop. The default FALSE will
#'   generate splits and operate on an expanded data.table in-memory. If your machine has
#'   limited memory, set this argument to TRUE in order to process splits sequentially.
#'   The default option should be much faster.
#'
#' @rdname splitting_functions
#' @import data.table
#' @import foreach
#' @export
make_conditional_splits <- function(cdvar = NULL, data, loop = FALSE) {
  if (is.null(cdvar)) {
    stop("To conduct conditional splitting, a conditioning variable must be specified.")
  }

  ## TODO: [2025-04-25] : add test
  CLEVELS <- sort(unique(data[, get(cdvar)]))
  if (length(CLEVELS) != 2) {
    stop("Conditioning variable must be binary.")
  }

  ## TODO: [2025-04-25] : add tests for conditional splitting routines
  NUM_DATA <- nrow(data)
  SPLIT1_SIZE <- floor(NUM_DATA / 2)
  L1_SIZE <- data[, sum(get(cdvar) == CLEVELS[2])]
  L0_SIZE <- data[, sum(get(cdvar) == CLEVELS[1])]
  SL11_SIZES <- seq_len(L1_SIZE - 1)
  SL10_SIZES <- sort(SPLIT1_SIZE - SL11_SIZES, decreasing = TRUE)

  if (loop == TRUE) {
    ## sample rows from original data according to spec
    spec <- data.table(sl_11_size = SL11_SIZES)
    spec[, sl_10_size := SPLIT1_SIZE - sl_11_size]
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
  } else {
    tmp <- data[rep(seq_len(.N), L1_SIZE - 1)]
    tmp[, iteration := rep(seq_len(L1_SIZE - 1), each = NUM_DATA)]
    tmp <- tmp[, .SD[sample(seq_len(.N))], keyby = .(iteration, dementia)]
    tmp[, split_id := unlist(lapply(SL11_SIZES, \(i) {
      splits0 <- rep(c(1, 2), c(SL10_SIZES[i], L0_SIZE - SL10_SIZES[i]))
      splits1 <- rep(c(1, 2), c(SL11_SIZES[i], L1_SIZE - SL11_SIZES[i]))
      c(splits0, splits1)
    }))]
  }

  tmp[]
}


#' Make a split dataset
# '
#' @param cdvar Character string naming auxiliary variable by which to condition splits
#' @param cdloop Boolean passed to `loop` argument of `make_conditional_splits()`. Ignored
#'   when conducting unconditional splits.
#' @import foreach
#'
#' @rdname splitting_functions
#' @export
make_splits <- function(cdvar = NULL, data, cdloop = FALSE, niter) {
  if (is.null(cdvar)) {
    tmpout <- make_unconditional_splits(data = data, niter = niter)
  } else {
    ## TODO: [2025-04-26] : add test for this message
    if (!is.null(niter)) {
      message("Ignoring `niter`, because the number of iterations will be determined ",
              "by the number of rows in the second level of the binary conditioning ",
              "variable.")
    }
    tmpout <- make_conditional_splits(cdvar = cdvar,
                                      loop = cdloop,
                                      data = data)
  }
  tmpout
}

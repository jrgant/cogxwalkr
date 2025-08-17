#' @title Splitting methods
#'
#' @description
#' These functions handle split routines used in the estimation of crosswalks. In general,
#' we recommend using these functions via [crosswalk()], but they are made available to
#' the user who wishes to call them directly.
#'   - [make_unconditional_splits()] : implements the unconditional split routine
#'   - [make_conditional_splits()] : implements the conditional split routine, in which
#'     splits in the data are defined by a binary variable (e.g., dementia) correlated
#'     with given cognitive measures (e.g., MMSE and MoCA) because the underlying
#'     cognitive construct is a common cause of all three
#'   - [make_splits()] : a handler that checks the input dataset and passes
#'     it on to the appropriate split function with relevant arguments specified
#'
#' @inheritParams crosswalk
#'
#' @rdname splitting_functions
#' @import data.table
#' @export
make_unconditional_splits <- function(data, niter = NULL) {

  if (is.null(niter)) {
    stop("The parameter `niter` must be set in order to do unconditional splits.")
  }

  data <- ingest_data(data)
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

#' @param cdvar Character string naming auxiliary variable by which to condition splits
#' @param loop Boolean declaring whether to use a for loop. The default FALSE will
#'   generate splits and operate on an expanded data.table in-memory. If your machine has
#'   limited memory, set this argument to TRUE in order to process splits sequentially.
#'   The default option should be much faster.
#' @rdname splitting_functions
#' @import data.table
#' @import foreach
#' @export
make_conditional_splits <- function(cdvar = NULL, data, loop = FALSE) {

  if (is.null(cdvar)) {
    stop("To conduct conditional splitting, a conditioning variable must be specified.")
  }

  data <- ingest_data(data)

  if (length(unique(data[[cdvar]])) != 2) {
    stop("Conditioning variable must be binary.")
  }
  CLEVELS <- sort(unique(data[[cdvar]]))

  NUM_DATA <- nrow(data)
  SPLIT1_SIZE <- floor(NUM_DATA / 2)
  L1_SIZE <- data[, sum(var == CLEVELS[2]), env = list(var = cdvar)]
  L0_SIZE <- data[, sum(var == CLEVELS[1]), env = list(var = cdvar)]
  SL11_SIZES <- seq_len(L1_SIZE - 1)
  SL10_SIZES <- sort(SPLIT1_SIZE - SL11_SIZES, decreasing = TRUE)

  if (loop == TRUE) {
    ## sample rows from original data according to spec
    spec <- data.table(sl_11_size = SL11_SIZES)
    spec[, sl_10_size := SPLIT1_SIZE - sl_11_size]
    tmp <- foreach(i = seq_len(nrow(spec)), .combine = rbind) %do% {
      shuffle <- spec[i, {
        lrows1 <- data[var == CLEVELS[2], env = list(var = cdvar)][sample(seq_len(.N))]
        lrows1[, split_id := rep(c(1, 2), times = c(sl_11_size, .N - sl_11_size))]
        lrows0 <- data[var == CLEVELS[1], env = list(var = cdvar)][sample(seq_len(.N))]
        lrows0[, split_id := rep(c(1, 2), times = c(sl_10_size, .N - sl_10_size))]
        rbind(lrows1, lrows0)[, iteration := i][]
      }]
      shuffle
    }
  } else {
    tmp <- data[rep(seq_len(.N), L1_SIZE - 1)]
    tmp[, iteration := rep(seq_len(L1_SIZE - 1), each = NUM_DATA)]
    tmp <- tmp[, .SD[sample(seq_len(.N))],
               keyby = list(iteration, var),
               env = list(var = cdvar)]
    tmp[, split_id := unlist(lapply(SL11_SIZES, \(i) {
      splits0 <- rep(c(1, 2), c(SL10_SIZES[i], L0_SIZE - SL10_SIZES[i]))
      splits1 <- rep(c(1, 2), c(SL11_SIZES[i], L1_SIZE - SL11_SIZES[i]))
      c(splits0, splits1)
    }))]
  }

  setcolorder(tmp, c("iteration", cdvar, "split_id"))
  setkeyv(tmp, c("iteration", cdvar, "split_id"))
  tmp[]
}

# Avoid R CMD check notes related to non-standard evaluation in data.table
utils::globalVariables(c("sl_10_size", "sl_11_size", "i"))


#' @param cdvar Character string naming auxiliary variable by which to condition splits
#' @param cdloop Boolean passed to `loop` argument of `make_conditional_splits()`. Ignored
#'   when conducting unconditional splits.
#'
#' @import foreach
#'
#' @rdname splitting_functions
#' @export
make_splits <- function(cdvar = NULL, data, cdloop = FALSE, niter = NULL) {

  data <- ingest_data(data)

  if (is.null(cdvar)) {
    tmpout <- make_unconditional_splits(data = data, niter = niter)
  } else {
    if (!is.null(niter)) {
      warning("Ignoring `niter`, which is used only for unconditional splits. ",
              "The number of iterations will be determined by the number of rows ",
              "in the second level of the conditioning variable (", cdvar, ").")
    }
    tmpout <- make_conditional_splits(cdvar = cdvar,
                                      loop = cdloop,
                                      data = data)
  }
  tmpout
}

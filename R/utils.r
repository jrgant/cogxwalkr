#' Check input data
#'
#' Used by multiple functions to check the class of input data and to
#' return the input as a data.table.
#'
#' @param data An input data object (data.table, data.frame, matrix)
ingest_data <- function(data) {

  if (is.data.table(data)) {
    data
  } else if (is.data.frame(data) || is.matrix(data) || is.list(data)) {
    as.data.table(data)
  } else {
    stop("The argument to `data` must be a data.frame, data.table, list, or matrix.")
  }

}

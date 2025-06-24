ingest_data <- function(data) {

  if (is.data.table(data)) {
    data
  } else if (is.data.frame(data) || is.matrix(data) || is.list(data)) {
    as.data.table(data)
  } else {
    stop("The argument to `data` must be a data.frame, data.table, list, or matrix.")
  }

}

test_that("ingest_data() handles input data correctly", {

  expect_error(ingest_data(data = c(1, 2, 3)))

  expect_s3_class(ingest_data(data = cogsim), "data.table")
  expect_s3_class(ingest_data(data = as.data.frame(cogsim)), "data.table")
  expect_s3_class(ingest_data(data = as.list(cogsim)), "data.table")
  expect_s3_class(ingest_data(data = as.matrix(cogsim)), "data.table")
  expect_s3_class(ingest_data(data = dplyr::as_tibble(cogsim)), "data.table")

})

test_that("crosswalk() disallows bad configurations", {

  # unconditional split indicated, but niter not set
  expect_error(crosswalk("mmse", "moca", cogsim))

  # input data must be data.frame or matrix
  expect_error(crosswalk("mmse", "moca", as.list(cogsim)))
})

test_that("crosswalk() disallows bad configurations", {

  # missing/incorrect variable name
  expect_error(crosswalk("mmse_1", "moca", cogsim))

  # input data must be data.frame(/table), matrix, or list
  expect_error(crosswalk("mmse", "moca", data = c(1, 2, 3)))
})

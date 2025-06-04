test_that("make_unconditional_splits() throws error when niter is null", {
  expect_error(make_unconditional_splits(cogsim))

  expect_error(
    crosswalk("mmse", "moca", cogsim)
  )
})

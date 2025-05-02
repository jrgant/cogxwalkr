test_that("crosswalk() disallows bad configurations", {

  # empty boot_control should produce error
  expect_error(
    crosswalk("mmse", "moca", cogsim, num_iter = 10, boot_ci = TRUE)
  )

  expect_error(
    crosswalk("mmse", "moca", as.list(cogsim), num_iter = 10)
  )

})

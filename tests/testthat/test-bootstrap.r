test_that("test control list validation", {
  expect_error(boot_control(nboot = NULL, seed = 123))
  expect_error(boot_control(nboot = "1", seed = 123))
  expect_error(boot_control(nboot = 100, seed = NULL, ncores = 1L))
  expect_error(boot_control(nboot = 100, seed = "123", ncores = 1L))
  expect_error(boot_control(nboot = 100, seed = 123, ncores = NULL))
  expect_error(boot_control(nboot = 100, seed = 123, ncores = "8"))
})

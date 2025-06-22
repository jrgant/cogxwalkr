test_that("plot method errors out if invalid type is provided", {
  cx <- crosswalk("mmse", "moca", cogsim)
  expect_error(plot(cx, types = "invalid"))
})

test_that("plot method errors out with bad requests based on crosswalk() output", {
  cx <- crosswalk("mmse", "moca", cogsim)
  expect_error(plot(cx, types = "invalid"))
  expect_error(plot(cx, types = "boot")) # no bootstrap results present in cx

  cxi <- crosswalk("mmse", "moca", cogsim, niter = 1000)
  expect_error(plot(cxi, types = "boot")) # diffs, but no bootstrap results present in cxi

  cxb <- crosswalk("mmse", "moca", cogsim, niter = 1000,
                   control = list(nboot = 10, seed = 123))
  expect_error(plot(cxb, cxsum = summary(cxb), citype = c("percentile", "normal")))
})

test_that("plot method behaves as intended for valid configurations", {
  cxb <- crosswalk("mmse", "moca", cogsim, control = list(nboot = 10, seed = 123))
  expect_message(plot(cxb, cxsum = summary(cxb))) # no split data should induce message

  cxb <- crosswalk("mmse", "moca", cogsim, niter = 1000,
                   control = list(nboot = 10, seed = 123))
  expect_no_failure(plot(cxb, cxsum = summary(cxb)))
})

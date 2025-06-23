test_that("crosswalk() disallows bad configurations", {

  # missing/incorrect variable name
  expect_error(crosswalk("mmse_1", "moca", cogsim))

  # input data must be data.frame(/table), matrix, or list
  expect_error(crosswalk("mmse", "moca", data = c(1, 2, 3)))
})

test_that("est_cw_coef() disallows bad configurations", {
  # methods allowed: lm, manual
  expect_error(est_cw_coef("mmse", "moca", cogsim, method = "glm"))
})

test_that("est_cw_coef() returns output of expected class", {
  out_lm <- est_cw_coef("mmse", "moca", cogsim, method = "lm")
  expect_s3_class(out_lm, "lm")

  out_dt <- est_cw_coef("mmse", "moca", cogsim, method = "manual")
  expect_s3_class(out_dt, "data.table")
})

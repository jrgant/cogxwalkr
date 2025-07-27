test_that("crosswalk() disallows bad inputs", {
  # missing/incorrect variable name
  expect_error(crosswalk("mmse_1", "moca", cogsim))
})

test_that("est_cw_coef() disallows bad inputs", {
  # methods allowed: lm, manual
  expect_error(est_cw_coef("mmse", "moca", cogsim, method = "glm"))
})

test_that("est_cw_coef() returns output of expected class", {
  out_lm <- est_cw_coef("mmse", "moca", cogsim, method = "lm")
  expect_s3_class(out_lm, "lm")

  out_dt <- est_cw_coef("mmse", "moca", cogsim, method = "manual")
  expect_s3_class(out_dt, "data.table")
})

test_that("crosswalk() disallows bad inputs", {
  # missing/incorrect variable name
  expect_error(crosswalk("mmse_1", "moca", cogsim))
})

test_that("crosswalk() returns expected outputs", {
  base_outnames <- c("cog1", "cog2", "fit", "diffs", "condition_var")

  cw1 <- crosswalk("mmse", "moca", cogsim)
  expect_true(is.null(cw1$condition_var))
  expect_true(is.null(cw1$diffs))
  expect_s3_class(cw1$fit, "lm")
  expect_identical(names(cw1), base_outnames)

  cw2 <- crosswalk("mmse", "moca", cogsim, niter = 500)
  expect_true(is.null(cw2$condition_var))
  expect_equal(nrow(cw2$diffs), 500)
  expect_identical(names(cw2), base_outnames)

  cw3 <- crosswalk("mmse", "moca", cogsim, condition_by = "dementia")
  expect_equal(cw3$condition_var, "dementia")
  expect_equal(nrow(cw3$diffs), sum(cogsim$dementia) - 1)
  expect_identical(names(cw3), base_outnames)

  cw4 <- crosswalk("mmse", "moca", cogsim, control = list(nboot = 100, seed = 1))
  expect_length(cw4$boot$dist, 100)
  expect_identical(names(cw4), c(base_outnames, "boot"))

  cw5 <- crosswalk("mmse", "moca", cogsim, condition_by = "dementia",
                   control = list(nboot = 100, seed = 1))
  expect_identical(names(cw5), c(base_outnames, "boot"))

  cwlist <- ls(pattern = "cw[0-9]")
  sapply(cwlist, \(.x) expect_equal(get(.x)[["cog1"]], "mmse"))
  sapply(cwlist, \(.x) expect_equal(get(.x)[["cog2"]], "moca"))
  sapply(cwlist, \(.x) expect_s3_class(get(.x)[["fit"]], "lm"))
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

test_that("do_crosswalk() handles inputs correctly", {
  cw <- crosswalk("mmse", "moca", cogsim)
  # est_mean cannot be NULL
  expect_error(do_crosswalk(cw))
  # warn if both `est_se` and `est_ci` are specified
  expect_warning(do_crosswalk(cw, est_mean = 5, est_se = 1.53, est_ci = c(2, 8)))
  # `est_ci` must be of length 2
  expect_error(do_crosswalk(cw, est_mean = 5, est_ci = 2))
  # must provide one of `est_se`, `est_ci`, or `est_pval`
  expect_error(do_crosswalk(cw, est_mean = 5))
})

test_that("do_crosswalk() calculates stats correctly and returns expected class", {
  cw <- crosswalk("mmse", "moca", cogsim)
  dcw1 <- do_crosswalk(cw, est_mean = 5, est_se = 1.53064)
  dcw2 <- do_crosswalk(cw, est_mean = 5, est_ci = c(2, 8))
  dcw3 <- do_crosswalk(cw, est_mean = 5, est_pval = 0.00109)

  TOL <- 0.01

  # manually entered and back-calculated SEs agree
  expect_equal(dcw1$estimate$se, dcw2$estimate$se, tolerance = TOL)
  expect_equal(dcw2$estimate$se, dcw3$estimate$se, tolerance = TOL)

  # crosswalked SEs agree
  expect_equal(dcw1$crosswalk$se, dcw2$crosswalk$se, tolerance = TOL)
  expect_equal(dcw2$crosswalk$se, dcw3$crosswalk$se, tolerance = TOL)

  # crosswalked CIs agree
  expect_equal(dcw1$crosswalk$ll, dcw2$crosswalk$ll, tolerance = TOL)
  expect_equal(dcw2$crosswalk$ll, dcw3$crosswalk$ll, tolerance = TOL)
  expect_equal(dcw1$crosswalk$ul, dcw2$crosswalk$ul, tolerance = TOL)
  expect_equal(dcw2$crosswalk$ul, dcw3$crosswalk$ul, tolerance = TOL)

  # check class
  expect_s3_class(dcw1, "cogxwalkr.crosswalk")
  expect_s3_class(dcw2, "cogxwalkr.crosswalk")
  expect_s3_class(dcw3, "cogxwalkr.crosswalk")
})

test_that("make_unconditional_splits() throws error when niter is null", {
  expect_error(make_unconditional_splits(cogsim))
})

test_that("make_conditional_splits() disallows bad inputs", {

  # must specific splitting variable
  expect_error(make_conditional_splits(data = cogsim))

  # input to cdvar must be of length=1
  expect_error(make_conditional_splits(cdvar = c("dementia", "age"), cogsim))
})

test_that("make_conditional_splits() executes with allowable inputs", {
  # default multi-threaded processing
  expect_no_error(make_conditional_splits("dementia", cogsim))
  expect_no_warning(make_conditional_splits("dementia", cogsim))
  expect_no_message(make_conditional_splits("dementia", cogsim))
  # sequential (looped) processed
  expect_no_error(make_conditional_splits("dementia", cogsim, loop = TRUE))
  expect_no_warning(make_conditional_splits("dementia", cogsim, loop = TRUE))
  expect_no_message(make_conditional_splits("dementia", cogsim, loop = TRUE))
})

test_that("make_conditional_splits() returns the expected object", {

  # default
  set.seed(1971)
  split <- make_conditional_splits("dementia", cogsim)
  expect_s3_class(split, "data.table")
  expect_equal(nrow(split), cogsim[, (sum(dementia) - 1) * .N])

  # sequential processing
  split_loop <- make_conditional_splits("dementia", cogsim, loop = TRUE)
  expect_equal(nrow(split), nrow(split_loop))

  expect_identical(
    split[, .N, keyby = .(iteration, dementia, split_id)],
    split_loop[, .N, keyby = .(iteration, dementia, split_id)]
  )
})

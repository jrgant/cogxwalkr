test_that("test control list validation", {
  expect_error(boot_control(nboot = NULL, seed = 123))
  expect_error(boot_control(nboot = "1", seed = 123))
  expect_error(boot_control(nboot = 100, seed = NULL, ncores = 1L))
  expect_error(boot_control(nboot = 100, seed = "123", ncores = 1L))
  expect_error(boot_control(nboot = 100, seed = 123, ncores = NULL))
  expect_error(boot_control(nboot = 100, seed = 123, ncores = "8"))
})

test_that("test that bootstrap_crosswalk() respects ncores request", {

  m1 <- capture_messages(
    bootstrap_crosswalk(
      cog1 = "mmse", cog2 = "moca", data = cogsim,
      nboot = 10, seed = 123, ncores = 1
    )
  )
  coreinfo1 <- m1[grepl("^Running", m1)] # select correct message
  expect_true(length(coreinfo1) == 1)
  ncores1 <- as.numeric(stringr::str_extract(coreinfo1, "[0-9]+"))
  expect_true(ncores1 == 1)

  m2 <- capture_messages(
    bootstrap_crosswalk(
      cog1 = "mmse", cog2 = "moca", data = cogsim,
      nboot = 10, seed = 123, ncores = 999
    )
  )
  coreinfo2 <- m2[grepl("^Running", m2)]
  expect_true(length(coreinfo2) == 1)
  ncores2 <- as.numeric(stringr::str_extract(coreinfo2, "[0-9]+"))
  expect_true(ncores2 >= 1 & ncores2 <= parallel::detectCores())
})

test_that("test that bootstrap_crosswalk() is reproducible across different `ncores`", {
  bootbase <- function(...) {
    bootstrap_crosswalk(
      cog1 = "mmse", cog2 = "moca", data = cogsim,
      nboot = 10, seed = 1066, ...
    )
  }
  b1 <- bootbase(ncores = 1)
  b2 <- bootbase(ncores = 2)
  b3 <- bootbase(ncores = 999)
  expect_identical(b1, b2)
  expect_identical(b2, b3)
})

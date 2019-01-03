context("test-distribution_super")

test_that("as_distribution works", {
  expect_error(as_distribution(list(1)), "li does not have name type")
  expect_error(as_distribution(list(type = "hiowqhi")), "li\\$type is missing 1 element from names\\(distributions\\)")
  expect_error(as_distribution(list(type = "uniform")), "li is missing 2 names from required_args")
  expect_error(as_distribution(list(type = "uniform", upper = 1, low = 4)), "li is missing 1 name from required_args")
  expect_error(as_distribution(list(type = "dddqd", lower = 1, upper = 2, mean = 1, sd = 2)), "li\\$type is missing 1 element from")

  expect_equal(as_distribution(list(type = "uniform", lower = 1L, upper = 2L)), uniform_distribution(lower = 1L, upper = 2L))
  expect_equal(as_distribution(list(type = "expuniform", lower = 1L, upper = 2L)), expuniform_distribution(lower = 1L, upper = 2L))
  expect_equal(as_distribution(list(type = "normal", mean = 1L, sd = 2L)), normal_distribution(mean = 1L, sd = 2L))
  expect_equal(as_distribution(list(type = "normal", mean = 1L, sd = 2L, lower = -4L, upper = 3L)), normal_distribution(mean = 1L, sd = 2L, lower = -4L, upper = 3L))
})

test_that("is_distribution works", {
  dis <- distribution(lower = 1, upper = 2, c = 3)
  expect_true(is_distribution(dis))

  lis <- list(lower = 1, upper = 2, c = 3)
  expect_false(is_distribution(lis))
})

test_that("print and cat works", {
  dis <- distribution(lower = 1, upper = 2, c = 3)
  expect_equal(as.character(dis), "distribution(lower = 1, upper = 2, c = 3)")
  expect_output(print(dis), "distribution\\(lower = 1, upper = 2, c = 3\\)")
})

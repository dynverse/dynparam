context("test-distribution_super")

test_that("list_as_distribution works", {
  expect_error(list_as_distribution(list(1)), "Unknown distribution list format")
  expect_error(list_as_distribution(list(class = "hiowqhi")), "Unknown distribution list format")
  expect_error(list_as_distribution(list(class = "uniform_distribution")), "Unknown distribution list format")
  expect_error(list_as_distribution(list(class = "uniform_distribution", upper = 1, low = 4)), "Unknown distribution list format")

  expect_equal(list_as_distribution(list(class = "uniform_distribution", lower = 1L, upper = 2L)), uniform_distribution(lower = 1L, upper = 2L))
  expect_equal(list_as_distribution(list(class = "expuniform_distribution", lower = 1L, upper = 2L)), expuniform_distribution(lower = 1L, upper = 2L))
  expect_equal(list_as_distribution(list(class = "normal_distribution", mean = 1L, sd = 2L)), normal_distribution(mean = 1L, sd = 2L))
  expect_equal(list_as_distribution(list(class = "normal_distribution", mean = 1L, sd = 2L, lower = -4L, upper = 3L)), normal_distribution(mean = 1L, sd = 2L, lower = -4L, upper = 3L))
})

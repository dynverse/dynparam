context("test-distribution_super")

test_that("as_distribution works", {
  expect_error(as_distribution(list(1)), "li does not have name class")
  expect_error(as_distribution(list(class = "hiowqhi")), "li\\$class is not an element of")
  expect_error(as_distribution(list(class = "uniform_distribution")), "li does not have names")
  expect_error(as_distribution(list(class = "uniform_distribution", upper = 1, low = 4)), "li does not have names")
  expect_error(as_distribution(list(class = "dddqd", lower = 1, upper = 2, mean = 1, sd = 2)), "li\\$class is not an element of")

  expect_equal(as_distribution(list(class = "uniform_distribution", lower = 1L, upper = 2L)), uniform_distribution(lower = 1L, upper = 2L))
  expect_equal(as_distribution(list(class = "expuniform_distribution", lower = 1L, upper = 2L)), expuniform_distribution(lower = 1L, upper = 2L))
  expect_equal(as_distribution(list(class = "normal_distribution", mean = 1L, sd = 2L)), normal_distribution(mean = 1L, sd = 2L))
  expect_equal(as_distribution(list(class = "normal_distribution", mean = 1L, sd = 2L, lower = -4L, upper = 3L)), normal_distribution(mean = 1L, sd = 2L, lower = -4L, upper = 3L))
})

test_that("is_distribution works", {
  dis <- distribution(a = 1, b = 2)
  expect_true(is_distribution(dis))

  lis <- list(a = 1, b = 2)
  expect_false(is_distribution(lis))
})

test_that("print and cat works", {
  dis <- distribution(a = 1, b = 2)
  expect_equal(as.character(dis), "distribution(a = 1, b = 2)")
  expect_output(print(dis), "distribution\\(a = 1, b = 2\\)")
})

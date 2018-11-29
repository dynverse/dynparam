context("test-distribution_super")

test_that("as_distribution works", {
  expect_error(as_distribution(list(1)), "li does not have name type")
  expect_error(as_distribution(list(type = "hiowqhi")), "li\\$type is not an element of")
  expect_error(as_distribution(list(type = "uniform")), "li does not have names")
  expect_error(as_distribution(list(type = "uniform", upper = 1, low = 4)), "li does not have names")
  expect_error(as_distribution(list(type = "dddqd", lower = 1, upper = 2, mean = 1, sd = 2)), "li\\$type is not an element of")

  expect_equal(as_distribution(list(type = "uniform", lower = 1L, upper = 2L)), uniform_distribution(lower = 1L, upper = 2L))
  expect_equal(as_distribution(list(type = "expuniform", lower = 1L, upper = 2L)), expuniform_distribution(lower = 1L, upper = 2L))
  expect_equal(as_distribution(list(type = "normal", mean = 1L, sd = 2L)), normal_distribution(mean = 1L, sd = 2L))
  expect_equal(as_distribution(list(type = "normal", mean = 1L, sd = 2L, lower = -4L, upper = 3L)), normal_distribution(mean = 1L, sd = 2L, lower = -4L, upper = 3L))
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

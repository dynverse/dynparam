context("test-assertions")

test_that("is_single_numeric works", {
  expect_true(is_single_numeric(23))
  expect_true(is_single_numeric(182L))
  expect_true(is_single_numeric(pi))

  expect_false(is_single_numeric("a"))
  expect_false(is_single_numeric(Inf))
  expect_false(is_single_numeric(-Inf))
  expect_false(is_single_numeric(NA))
  expect_false(is_single_numeric(NaN))
  expect_false(is_single_numeric(NULL))
  expect_false(is_single_numeric(c(1,2)))
  expect_false(is_single_numeric(1:10))
  expect_false(is_single_numeric(~ help_me + im_trapped + in_a_unit_test))
  expect_false(is_single_numeric(function(x) "call 911 now"))
})

test_that("is_single_numeric with allow_neg_inf works", {
  expect_true(is_single_numeric(23, allow_neg_inf = TRUE))
  expect_true(is_single_numeric(182L, allow_neg_inf = TRUE))
  expect_true(is_single_numeric(pi, allow_neg_inf = TRUE))
  expect_true(is_single_numeric(-Inf, allow_neg_inf = TRUE))

  expect_false(is_single_numeric("a", allow_neg_inf = TRUE))
  expect_false(is_single_numeric(Inf, allow_neg_inf = TRUE))
  expect_false(is_single_numeric(NA, allow_neg_inf = TRUE))
  expect_false(is_single_numeric(NaN, allow_neg_inf = TRUE))
  expect_false(is_single_numeric(NULL, allow_neg_inf = TRUE))
  expect_false(is_single_numeric(c(1,2, allow_neg_inf = TRUE)))
  expect_false(is_single_numeric(1:10, allow_neg_inf = TRUE))
  expect_false(is_single_numeric(~ help_me + im_trapped + in_a_unit_test, allow_neg_inf = TRUE))
  expect_false(is_single_numeric(function(x) "call 911 now", allow_neg_inf = TRUE))
})

test_that("is_single_numeric with allow_pos_inf works", {
  expect_true(is_single_numeric(23, allow_pos_inf = TRUE))
  expect_true(is_single_numeric(182L, allow_pos_inf = TRUE))
  expect_true(is_single_numeric(pi, allow_pos_inf = TRUE))
  expect_true(is_single_numeric(Inf, allow_pos_inf = TRUE))

  expect_false(is_single_numeric("a", allow_pos_inf = TRUE))
  expect_false(is_single_numeric(-Inf, allow_pos_inf = TRUE))
  expect_false(is_single_numeric(NA, allow_pos_inf = TRUE))
  expect_false(is_single_numeric(NaN, allow_pos_inf = TRUE))
  expect_false(is_single_numeric(NULL, allow_pos_inf = TRUE))
  expect_false(is_single_numeric(c(1,2, allow_pos_inf = TRUE)))
  expect_false(is_single_numeric(1:10, allow_pos_inf = TRUE))
  expect_false(is_single_numeric(~ help_me + im_trapped + in_a_unit_test, allow_pos_inf = TRUE))
  expect_false(is_single_numeric(function(x) "call 911 now", allow_pos_inf = TRUE))
})

test_that("has_names works", {
  expect_true(list(a = 1, b = 2) %has_names% "a")
  expect_true(list(a = 1, b = 2) %has_names% c("a", "b"))
  expect_false(list(a = 1, b = 2) %has_names% "c")

  expect_true(c(a = 1, b = 2) %has_names% "a")
  expect_true(c(a = 1, b = 2) %has_names% c("a", "b"))
  expect_false(c(a = 1, b = 2) %has_names% "c")
})

context("test-helper_check_finite")

test_that("check_finite works", {
  expect_true(check_finite(23))
  expect_true(check_finite(182L))
  expect_true(check_finite(pi))

  expect_false(check_finite("a"))
  expect_false(check_finite(Inf))
  expect_false(check_finite(-Inf))
  expect_false(check_finite(NA))
  expect_false(check_finite(NaN))
  expect_false(check_finite(NULL))
  expect_false(check_finite(c(1,2)))
  expect_false(check_finite(1:10))
  expect_false(check_finite(~ help_me + im_trapped + in_a_unit_test))
  expect_false(check_finite(function(x) "call 911 now"))
})

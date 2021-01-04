context("test-helper_collapse_set")

test_that("collapse_set works", {
  expect_equal(collapse_set("AaAaAa"), "AaAaAa", check.environment = FALSE)

  expect_equal(collapse_set(letters[1:5]), "{a, b, c, d, e}", check.environment = FALSE)

  expect_equal(collapse_set("a", "B", "c", "d", 1, 8L), "{a, B, c, d, 1, 8}", check.environment = FALSE)

  expect_equal(collapse_set(list("a", "B", "c"), c(1, 8L)), "{a, B, c, 1, 8}", check.environment = FALSE)

  expect_equal(collapse_set(), "{}", check.environment = FALSE)

  expect_equal(collapse_set(456789), "456789", check.environment = FALSE)

  expect_equal(collapse_set(c(1, 2, NA)), "{1, 2, NA}", check.environment = FALSE)

  expect_equal(collapse_set(1, "b", list("Ree", 1.5), prefix = "~", postfix = "_", sep = "#"), "~1#b#Ree#1.5_", check.environment = FALSE)
})

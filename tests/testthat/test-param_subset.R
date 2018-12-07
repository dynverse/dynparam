context("test-param_subset")

test_that("correlation method test", {
  p <- subset_parameter(
    id = "method",
    default = "pearson",
    values = c("pearson", "spearman", "kendall"),
    description = "Which correlation coefficient to compute."
  )

  expect_is(p, "subset_parameter")
  expect_equal(p$id, "method")
  expect_equal(p$default, "pearson")
  expect_equal(p$values, c("pearson", "spearman", "kendall"))
  expect_equal(p$description, "Which correlation coefficient to compute.")

  expect_match(as.character(p), "subset")
  expect_match(as.character(p), "method")
  expect_match(as.character(p), "\\{pearson, spearman, kendall\\}")
  expect_match(as.character(p), "default=pearson")

  li <- as.list(p)

  expect_equal(li$type, "subset")
  expect_equal(li$id, "method")
  expect_equal(li$default, "pearson")
  expect_equal(li$values, c("pearson", "spearman", "kendall"))
  expect_equal(li$description, "Which correlation coefficient to compute.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  expect_equal(ph$id, "method")
  expect_equal(ph$default %>% unlist(), c("TRUE", "FALSE", "FALSE"))
  expect_equal(ph$len, 3)
})

test_that("wrong parse fails gracefully", {
  expect_error(subset_parameter(id = "a", default = "b", values = "c", description = "d"), "default is missing elements \"c\"")
  expect_error(subset_parameter(id = "a", default = "b", values = c("c", "d"), description = "d"), "default is missing elements c\\(\"c\", \"d\"\\)")
  expect_error(subset_parameter(id = 1, default = "b", values = "b", description = "d"), "id is not a character vector")
  expect_error(subset_parameter(id = "a", default = 1, values = "b", description = "d"), "default is not a character vector")
  expect_error(subset_parameter(id = "a", default = "b", values = 1, description = "d"), "values is not a character vector")
  expect_error(subset_parameter(id = "a", default = "b", values = "b", description = 1), "description is not NULL or description is not a character vector")
})


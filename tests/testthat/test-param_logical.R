context("test-param_logical")

test_that("scale test", {
  p <- logical_parameter(
    id = "scale",
    default = TRUE,
    description = "Whether or not to scale the input variables"
  )

  expect_is(p, "logical_parameter")
  expect_equal(p$id, "scale", check.environment = FALSE)
  expect_equal(p$default, TRUE, check.environment = FALSE)
  expect_equal(p$description, "Whether or not to scale the input variables", check.environment = FALSE)

  expect_match(as.character(p), "logical")
  expect_match(as.character(p), "scale")
  expect_match(as.character(p), "default=TRUE")

  li <- as.list(p)

  expect_equal(li$type, "logical", check.environment = FALSE)
  expect_equal(li$id, "scale", check.environment = FALSE)
  expect_equal(li$default, TRUE, check.environment = FALSE)
  expect_equal(li$description, "Whether or not to scale the input variables", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "scale", check.environment = FALSE)
  expect_equal(ph$default, TRUE, check.environment = FALSE)
  expect_equal(ph$len, 1, check.environment = FALSE)
})

test_that("multiple value test", {
  p <- logical_parameter(
    id = "num_iter",
    default = c(TRUE, FALSE, TRUE),
    description = "The number of iterations."
  )

  expect_equal(p$id, "num_iter", check.environment = FALSE)
  expect_equal(p$default, c(TRUE, FALSE, TRUE), check.environment = FALSE)
  expect_equal(p$description, "The number of iterations.", check.environment = FALSE)

  expect_match(as.character(p), "logical")
  expect_match(as.character(p), "num_iter")
  expect_match(as.character(p), "default=\\{TRUE, FALSE, TRUE\\}")

  li <- as.list(p)

  expect_equal(li$type, "logical", check.environment = FALSE)
  expect_equal(li$id, "num_iter", check.environment = FALSE)
  expect_equal(li$default, c(TRUE, FALSE, TRUE), check.environment = FALSE)
  expect_equal(li$description, "The number of iterations.", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "num_iter", check.environment = FALSE)
  expect_equal(ph$default, c(TRUE, FALSE, TRUE), check.environment = FALSE)
  expect_equal(ph$len, 3, check.environment = FALSE)
})

test_that("wrong parse fails gracefully", {
  expect_error(logical_parameter(id = 1, default = TRUE, description = "d"), "id is not a character vector")
  expect_error(logical_parameter(id = "a", default = "TrUe", description = "d"), "is.logical\\(default\\) is not TRUE") # ??!
  expect_error(logical_parameter(id = "a", default = TRUE,  description = 3), "description is not NULL or description is not a character vector")
})


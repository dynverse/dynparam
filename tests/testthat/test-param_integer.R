context("test-param_integer")

test_that("k cluster test", {
  p <- integer_parameter(
    id = "k",
    default = 5L,
    distribution = uniform_distribution(3L, 10L),
    description = "The number of clusters."
  )

  expect_is(p, "integer_parameter")
  expect_equal(p$id, "k", check.environment = FALSE)
  expect_equal(p$default, 5L, check.environment = FALSE)
  expect_equal(p$distribution, uniform_distribution(3L, 10L), check.environment = FALSE)
  expect_equal(p$description, "The number of clusters.", check.environment = FALSE)

  expect_match(as.character(p), "integer")
  expect_match(as.character(p), "k")
  expect_match(as.character(p), "U\\(3, 10\\)")
  expect_match(as.character(p), "default=5")

  li <- as.list(p)

  expect_equal(li$type, "integer", check.environment = FALSE)
  expect_equal(li$id, "k", check.environment = FALSE)
  expect_equal(li$default, 5L, check.environment = FALSE)
  expect_equal(li$distribution, as.list(uniform_distribution(3L, 10L)), check.environment = FALSE)
  expect_equal(li$description, "The number of clusters.", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "k", check.environment = FALSE)
  expect_equal(ph$default, (5L-3L) / (10L-3L), check.environment = FALSE)
  expect_equal(ph$lower, 0, check.environment = FALSE)
  expect_equal(ph$upper, 1, check.environment = FALSE)
  expect_equal(ph$len, 1, check.environment = FALSE)
})

test_that("multiple value test", {
  p <- integer_parameter(
    id = "num_iter",
    default = c(100L, 1000L),
    distribution = expuniform_distribution(10L, 10000L),
    description = "The number of iterations."
  )

  expect_equal(p$id, "num_iter", check.environment = FALSE)
  expect_equal(p$default, c(100L, 1000L), check.environment = FALSE)
  expect_equal(p$distribution, expuniform_distribution(10L, 10000L), check.environment = FALSE)
  expect_equal(p$description, "The number of iterations.", check.environment = FALSE)

  expect_match(as.character(p), "integer")
  expect_match(as.character(p), "num_iter")
  expect_match(as.character(p), "e\\^U\\(2.30, 9.21\\)")
  expect_match(as.character(p), "default=\\{100, 1000\\}")

  li <- as.list(p)

  expect_equal(li$type, "integer", check.environment = FALSE)
  expect_equal(li$id, "num_iter", check.environment = FALSE)
  expect_equal(li$default, c(100L, 1000L), check.environment = FALSE)
  expect_equal(li$distribution, as.list(expuniform_distribution(10L, 10000L)), check.environment = FALSE)
  expect_equal(li$description, "The number of iterations.", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "num_iter", check.environment = FALSE)
  expect_equal(unlist(unname(ph$default)), (log(c(100L, 1000L)) - log(10L)) / (log(10000L) - log(10L)), check.environment = FALSE)
  expect_equal(ph$lower, c(0, 0), check.environment = FALSE)
  expect_equal(ph$upper, c(1, 1), check.environment = FALSE)
  expect_equal(ph$len, 2, check.environment = FALSE)
})

test_that("wrong parse fails gracefully", {
  expect_error(integer_parameter(id = 1, default = 1, distribution = normal_distribution(1, 2), description = "d"), "id is not a character vector")
  expect_error(integer_parameter(id = "a", default = "1", distribution = normal_distribution(1, 2), description = "d"), "default is not a numeric or integer vector")
  expect_error(integer_parameter(id = "a", default = 1, distribution = "normal_distribution(1, 2)", description = "d"), "distribution is not a distribution")
  expect_error(integer_parameter(id = "a", default = 1, distribution = normal_distribution(1, 2), description = 3L), "description is not NULL or description is not a character vector")
})


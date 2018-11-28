context("test-param_integer")

test_that("k cluster test", {
  p <- integer_parameter(
    id = "k",
    default = 5L,
    distribution = uniform_distribution(3L, 10L),
    description = "The number of clusters."
  )

  expect_is(p, "integer_parameter")
  expect_equal(p$id, "k")
  expect_equal(p$default, 5L)
  expect_equal(p$distribution, uniform_distribution(3L, 10L))
  expect_equal(p$description, "The number of clusters.")

  expect_match(as.character(p), "integer")
  expect_match(as.character(p), "k")
  expect_match(as.character(p), "U\\(3, 10\\)")
  expect_match(as.character(p), "default=5")

  li <- as.list(p)

  expect_equal(li$class, "integer_parameter")
  expect_equal(li$id, "k")
  expect_equal(li$default, 5L)
  expect_equal(li$distribution, as.list(uniform_distribution(3L, 10L)))
  expect_equal(li$description, "The number of clusters.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "k")
  expect_equal(ph1$default, (5L-3L) / (10L-3L))
  expect_equal(ph1$lower, 0)
  expect_equal(ph1$upper, 1)
  expect_equal(ph1$len, 1)
})

test_that("multiple value test", {
  p <- integer_parameter(
    id = "num_iter",
    default = c(100L, 1000L),
    distribution = expuniform_distribution(10L, 10000L),
    description = "The number of iterations."
  )

  expect_equal(p$id, "num_iter")
  expect_equal(p$default, c(100L, 1000L))
  expect_equal(p$distribution, expuniform_distribution(10L, 10000L))
  expect_equal(p$description, "The number of iterations.")

  expect_match(as.character(p), "integer")
  expect_match(as.character(p), "num_iter")
  expect_match(as.character(p), "e\\^U\\(2.30, 9.21\\)")
  expect_match(as.character(p), "default=\\{100, 1000\\}")

  li <- as.list(p)

  expect_equal(li$class, "integer_parameter")
  expect_equal(li$id, "num_iter")
  expect_equal(li$default, c(100L, 1000L))
  expect_equal(li$distribution, as.list(expuniform_distribution(10L, 10000L)))
  expect_equal(li$description, "The number of iterations.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "num_iter")
  expect_equal(unlist(unname(ph1$default)), (log(c(100L, 1000L)) - log(10L)) / (log(10000L) - log(10L)))
  expect_equal(ph1$lower, c(0, 0))
  expect_equal(ph1$upper, c(1, 1))
  expect_equal(ph1$len, 2)
})

test_that("wrong parse fails gracefully", {
  expect_error(integer_parameter(id = 1, default = 1, distribution = normal_distribution(1, 2), description = "d"), "id is not a character vector")
  expect_error(integer_parameter(id = "a", default = "1", distribution = normal_distribution(1, 2), description = "d"), "default is not a numeric or integer vector")
  expect_error(integer_parameter(id = "a", default = 1, distribution = "normal_distribution(1, 2)", description = "d"), "distribution is not a distribution")
  expect_error(integer_parameter(id = "a", default = 1, distribution = normal_distribution(1, 2), description = 3L), "description is not NULL or description is not a character vector")
})


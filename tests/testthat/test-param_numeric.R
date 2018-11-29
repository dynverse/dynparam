context("test-param_numeric")

test_that("k cluster test", {
  p <- numeric_parameter(
    id = "k",
    default = 5,
    distribution = uniform_distribution(3, 10),
    description = "The number of clusters."
  )

  expect_is(p, "numeric_parameter")
  expect_equal(p$id, "k")
  expect_equal(p$default, 5)
  expect_equal(p$distribution, uniform_distribution(3, 10))
  expect_equal(p$description, "The number of clusters.")

  expect_match(as.character(p), "numeric")
  expect_match(as.character(p), "k")
  expect_match(as.character(p), "U\\(3, 10\\)")
  expect_match(as.character(p), "default=5")

  li <- as.list(p)

  expect_equal(li$type, "numeric")
  expect_equal(li$id, "k")
  expect_equal(li$default, 5)
  expect_equal(li$distribution, as.list(uniform_distribution(3, 10)))
  expect_equal(li$description, "The number of clusters.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "k")
  expect_equal(ph1$default, (5-3) / (10-3))
  expect_equal(ph1$lower, 0)
  expect_equal(ph1$upper, 1)
  expect_equal(ph1$len, 1)
})

test_that("multiple value test", {
  p <- numeric_parameter(
    id = "num_iter",
    default = c(100, 1000),
    distribution = expuniform_distribution(10, 10000),
    description = "The number of iterations."
  )

  expect_equal(p$id, "num_iter")
  expect_equal(p$default, c(100, 1000))
  expect_equal(p$distribution, expuniform_distribution(10, 10000))
  expect_equal(p$description, "The number of iterations.")

  expect_match(as.character(p), "numeric")
  expect_match(as.character(p), "num_iter")
  expect_match(as.character(p), "e\\^U\\(2.30, 9.21\\)")
  expect_match(as.character(p), "default=\\{100, 1000\\}")

  li <- as.list(p)

  expect_equal(li$type, "numeric")
  expect_equal(li$id, "num_iter")
  expect_equal(li$default, c(100, 1000))
  expect_equal(li$distribution, as.list(expuniform_distribution(10, 10000)))
  expect_equal(li$description, "The number of iterations.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "num_iter")
  expect_equal(unlist(unname(ph1$default)), (log(c(100, 1000)) - log(10)) / (log(10000) - log(10)))
  expect_equal(ph1$lower, c(0, 0))
  expect_equal(ph1$upper, c(1, 1))
  expect_equal(ph1$len, 2)
})

test_that("wrong parse fails gracefully", {
  expect_error(numeric_parameter(id = 1, default = 1, distribution = normal_distribution(1, 2), description = "d"), "id is not a character vector")
  expect_error(numeric_parameter(id = "a", default = "1", distribution = normal_distribution(1, 2), description = "d"), "default is not a numeric or integer vector")
  expect_error(numeric_parameter(id = "a", default = 1, distribution = "normal_distribution(1, 2)", description = "d"), "distribution is not a distribution")
  expect_error(numeric_parameter(id = "a", default = 1, distribution = normal_distribution(1, 2), description = 3), "description is not NULL or description is not a character vector")
})


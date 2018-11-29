context("test-param_range")

test_that("k cluster test", {
  p <- range_parameter(
    id = "ks",
    default = c(3L, 15L),
    lower_distribution = uniform_distribution(2L, 5L),
    upper_distribution = uniform_distribution(10L, 20L),
    description = "The number of clusters."
  )

  expect_is(p, "range_parameter")
  expect_equal(p$id, "ks")
  expect_true(p$as_integer)
  expect_equal(p$default, c(3L, 15L))
  expect_equal(p$lower_distribution, uniform_distribution(2L, 5L))
  expect_equal(p$upper_distribution, uniform_distribution(10L, 20L))
  expect_equal(p$description, "The number of clusters.")

  expect_match(as.character(p), "range")
  expect_match(as.character(p), "ks")
  expect_match(as.character(p), "U\\(2, 5\\)")
  expect_match(as.character(p), "U\\(10, 20\\)")
  expect_match(as.character(p), "default=\\(3, *15\\)")

  li <- as.list(p)

  expect_equal(li$type, "range")
  expect_equal(li$id, "ks")
  expect_true(li$as_integer)
  expect_equal(li$default, c(3L, 15L))
  expect_equal(li$lower_distribution, as.list(uniform_distribution(2L, 5L)))
  expect_equal(li$upper_distribution, as.list(uniform_distribution(10L, 20L)))
  expect_equal(li$description, "The number of clusters.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "ks_lower")
  expect_equal(ph1$default, (3L-2L) / (5L-2L))
  expect_equal(ph1$lower, 0)
  expect_equal(ph1$upper, 1)
  expect_equal(ph1$len, 1)

  ph2 <- ph$params[[2]]

  expect_equal(ph2$id, "ks_upper")
  expect_equal(ph2$default, (15L-10L) / (20L-10L))
  expect_equal(ph2$lower, 0)
  expect_equal(ph2$upper, 1)
  expect_equal(ph2$len, 1)
})


test_that("quantiles test", {
  p <- range_parameter(
    id = "quantiles",
    as_integer = FALSE,
    default = c(0.1, 0.99),
    lower_distribution = uniform_distribution(0.01, 0.25),
    upper_distribution = uniform_distribution(0.9, 0.99),
    description = "The lower and upper quantile thresholds."
  )

  expect_is(p, "range_parameter")
  expect_equal(p$id, "quantiles")
  expect_false(p$as_integer)
  expect_equal(p$default, c(.1, .99))
  expect_equal(p$lower_distribution, uniform_distribution(0.01, .25))
  expect_equal(p$upper_distribution, uniform_distribution(.9, .99))
  expect_equal(p$description, "The lower and upper quantile thresholds.")

  expect_match(as.character(p), "range")
  expect_match(as.character(p), "quantiles")
  expect_match(as.character(p), "U\\(0.01, 0.25\\)")
  expect_match(as.character(p), "U\\(0.9, 0.99\\)")
  expect_match(as.character(p), "default=\\(0.1, *0.99\\)")

  li <- as.list(p)

  expect_equal(li$type, "range")
  expect_false(li$as_integer)
  expect_equal(li$id, "quantiles")
  expect_equal(li$default, c(0.1, 0.99))
  expect_equal(li$lower_distribution, as.list(uniform_distribution(0.01, 0.25)))
  expect_equal(li$upper_distribution, as.list(uniform_distribution(0.9, 0.99)))
  expect_equal(li$description, "The lower and upper quantile thresholds.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)
  ph1 <- ph$params[[1]]

  expect_equal(ph1$id, "quantiles_lower")
  expect_equal(ph1$default, (.1-.01) / (.25-.01))
  expect_equal(ph1$lower, 0)
  expect_equal(ph1$upper, 1)
  expect_equal(ph1$len, 1)

  ph2 <- ph$params[[2]]

  expect_equal(ph2$id, "quantiles_upper")
  expect_equal(ph2$default, (.99-.9) / (.99-.9))
  expect_equal(ph2$lower, 0)
  expect_equal(ph2$upper, 1)
  expect_equal(ph2$len, 1)
})



test_that("wrong parse fails gracefully", {
  ld <- uniform_distribution(2L, 5L)
  ud <- uniform_distribution(10L, 20L)
  expect_error(range_parameter(id = 1, default = c(3L, 15L), lower_distribution = ld, upper_distribution = ud, description = "d"), "id is not a character vector")
  expect_error(range_parameter(id = "a", default = "1", lower_distribution = ld, upper_distribution = ud, description = "d"), "default is not a numeric or integer vector")
  expect_error(range_parameter(id = "a", default = 1, lower_distribution = "ld", upper_distribution = ud, description = "d"), "lower_distribution is not a distribution")
  expect_error(range_parameter(id = "a", default = 1, lower_distribution = ld, upper_distribution = "ud", description = "d"), "upper_distribution is not a distribution")
  expect_error(range_parameter(id = "a", default = 1, lower_distribution = ld, upper_distribution = ud, description = 3L), "description is not NULL or description is not a character vector")
})


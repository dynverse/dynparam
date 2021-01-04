context("test-param_numeric_range")

test_that("quantiles test", {
  p <- numeric_range_parameter(
    id = "quantiles",
    default = c(0.1, 0.99),
    lower_distribution = uniform_distribution(0.01, 0.25),
    upper_distribution = uniform_distribution(0.9, 0.99),
    description = "The lower and upper quantile thresholds."
  )

  expect_is(p, "numeric_range_parameter")
  expect_equal(p$id, "quantiles", check.environment = FALSE)
  expect_equal(p$default, c(.1, .99), check.environment = FALSE)
  expect_equal(p$lower_distribution, uniform_distribution(0.01, .25), check.environment = FALSE)
  expect_equal(p$upper_distribution, uniform_distribution(.9, .99), check.environment = FALSE)
  expect_equal(p$description, "The lower and upper quantile thresholds.", check.environment = FALSE)

  expect_match(as.character(p), "numeric_range")
  expect_match(as.character(p), "quantiles")
  expect_match(as.character(p), "U\\(0.01, 0.25\\)")
  expect_match(as.character(p), "U\\(0.9, 0.99\\)")
  expect_match(as.character(p), "default=\\(0.1, *0.99\\)")

  li <- as.list(p)

  expect_equal(li$type, "numeric_range", check.environment = FALSE)
  expect_equal(li$id, "quantiles", check.environment = FALSE)
  expect_equal(li$default, c(0.1, 0.99), check.environment = FALSE)
  expect_equal(li$lower_distribution, as.list(uniform_distribution(0.01, 0.25)), check.environment = FALSE)
  expect_equal(li$upper_distribution, as.list(uniform_distribution(0.9, 0.99)), check.environment = FALSE)
  expect_equal(li$description, "The lower and upper quantile thresholds.", check.environment = FALSE)

  p2 <- as_parameter(li)
  expect_equal(p2, p, check.environment = FALSE)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "quantiles", check.environment = FALSE)
  expect_equal(ph$default, c((.1-.01) / (.25-.01), (.99-.9) / (.99-.9)), check.environment = FALSE)
  expect_equal(ph$lower, c(0, 0), check.environment = FALSE)
  expect_equal(ph$upper, c(1, 1), check.environment = FALSE)
  expect_equal(ph$len, 2, check.environment = FALSE)

  ps <- ParamHelpers::makeParamSet(ph)
  tval <-
    ParamHelpers::generateDesign(par.set = ps, n = 1) %>%
    ParamHelpers::dfRowToList(par.set = ps, i = 1) %>%
    ParamHelpers::trafoValue(par = ps, .)

  expect_equal(names(tval), "quantiles", check.environment = FALSE)
  expect_gte(tval$quantiles[[1]], .01)
  expect_lte(tval$quantiles[[1]], .25)
  expect_gte(tval$quantiles[[2]], .9)
  expect_lte(tval$quantiles[[2]], .99)
  expect_true(tval$quantiles[[1]] <= tval$quantiles[[2]])
})

test_that("wrong parse fails gracefully", {
  ld <- uniform_distribution(2L, 5L)
  ud <- uniform_distribution(10L, 20L)
  expect_error(numeric_range_parameter(id = 1, default = c(3L, 15L), lower_distribution = ld, upper_distribution = ud, description = "d"), "id is not a character vector")
  expect_error(numeric_range_parameter(id = "a", default = "1", lower_distribution = ld, upper_distribution = ud, description = "d"), "default is not a numeric or integer vector")
  expect_error(numeric_range_parameter(id = "a", default = 1, lower_distribution = "ld", upper_distribution = ud, description = "d"), "lower_distribution is not a distribution")
  expect_error(numeric_range_parameter(id = "a", default = 1, lower_distribution = ld, upper_distribution = "ud", description = "d"), "upper_distribution is not a distribution")
  expect_error(numeric_range_parameter(id = "a", default = 1, lower_distribution = ld, upper_distribution = ud, description = 3L), "description is not NULL or description is not a character vector")
})


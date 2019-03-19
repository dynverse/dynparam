context("test-param_integer_range")

test_that("k cluster test", {
  p <- integer_range_parameter(
    id = "ks",
    default = c(3L, 15L),
    lower_distribution = uniform_distribution(2L, 5L),
    upper_distribution = uniform_distribution(10L, 20L),
    description = "The number of clusters."
  )

  expect_is(p, "integer_range_parameter")
  expect_equal(p$id, "ks")
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

  expect_equal(li$type, "integer_range")
  expect_equal(li$id, "ks")
  expect_equal(li$default, c(3L, 15L))
  expect_equal(li$lower_distribution, as.list(uniform_distribution(2L, 5L)))
  expect_equal(li$upper_distribution, as.list(uniform_distribution(10L, 20L)))
  expect_equal(li$description, "The number of clusters.")

  p2 <- as_parameter(li)
  expect_equal(p2, p)

  ph <- as_paramhelper(p)

  expect_equal(ph$id, "ks")
  expect_equal(ph$default, c((3L-2L) / (5L-2L), (15L-10L) / (20L-10L)))
  expect_equal(ph$lower, c(0, 0))
  expect_equal(ph$upper, c(1, 1))
  expect_equal(ph$len, 2)

  ps <- ParamHelpers::makeParamSet(ph)
  tval <-
    ParamHelpers::generateDesign(par.set = ps, n = 1) %>%
    ParamHelpers::dfRowToList(par.set = ps, i = 1) %>%
    ParamHelpers::trafoValue(par = ps, .)

  expect_equal(names(tval), "ks")
  expect_gte(tval$ks[[1]], 2)
  expect_lte(tval$ks[[1]], 5)
  expect_gte(tval$ks[[2]], 10)
  expect_lte(tval$ks[[2]], 20)
  expect_true(tval$ks[[1]] <= tval$ks[[2]])
})



test_that("wrong parse fails gracefully", {
  ld <- uniform_distribution(2L, 5L)
  ud <- uniform_distribution(10L, 20L)
  expect_error(integer_range_parameter(id = 1, default = c(3L, 15L), lower_distribution = ld, upper_distribution = ud, description = "d"), "id is not a character vector")
  expect_error(integer_range_parameter(id = "a", default = "1", lower_distribution = ld, upper_distribution = ud, description = "d"), "default is not a numeric or integer vector")
  expect_error(integer_range_parameter(id = "a", default = 1, lower_distribution = "ld", upper_distribution = ud, description = "d"), "lower_distribution is not a distribution")
  expect_error(integer_range_parameter(id = "a", default = 1, lower_distribution = ld, upper_distribution = "ud", description = "d"), "upper_distribution is not a distribution")
  expect_error(integer_range_parameter(id = "a", default = 1, lower_distribution = ld, upper_distribution = ud, description = 3L), "description is not NULL or description is not a character vector")
})


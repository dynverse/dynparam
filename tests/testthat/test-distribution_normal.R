context("test-distribution_normal")

test_that("normal_distribution works with fixed values", {
  dis <- normal_distribution(mean = 50, sd = 10)
  expect_match(as.character(dis), "N\\(50[\\.0]*, 10[\\.0]*\\)")
})

test_that("normal_distribution works with random values", {
  mean <- runif(1, 50, 100)
  sd <- runif(1, 5, 10)

  # create distribution
  dis <- normal_distribution(mean = mean, sd = sd)
  expect_lte(abs(dis$mean - mean), 1e-5)
  expect_lte(abs(dis$sd - sd), 1e-5)

  # transform to list
  lis <- as_list(dis)
  expect_equal(lis$class, class(dis)[[1]])
  expect_lte(abs(lis$mean - mean), 1e-5)
  expect_lte(abs(lis$sd - sd), 1e-5)

  # transform back to distribution
  dis2 <- list_as_distribution(lis)
  expect_equal(class(dis2), class(dis))
  expect_lte(abs(dis2$mean - mean), 1e-5)
  expect_lte(abs(dis2$sd - sd), 1e-5)

  # test dist and quan functions
  dfun <- distribution_function(dis)
  qfun <- quantile_function(dis)

  expect_equal(qfun(0), -Inf)
  expect_lte(abs(qfun(0.5) - mean), 1e-5)
  expect_equal(qfun(1), Inf)
  expect_true(all(abs(dfun(c(-Inf, mean, Inf)) - c(0, .5, 1)) < 1e-5))
})

test_that("normal_distribution with limits works with random values", {
  mean <- runif(1, 50, 100)
  sd <- runif(1, 5, 10)
  lower <- runif(1, 0, 49)
  upper <- runif(1, 101, 200)

  # create distribution
  dis <- normal_distribution(mean = mean, sd = sd, lower = lower, upper = upper)
  expect_lte(abs(dis$lower - lower), 1e-5)
  expect_lte(abs(dis$upper - upper), 1e-5)

  # transform to list
  lis <- as_list(dis)
  expect_equal(lis$class, class(dis)[[1]])
  expect_lte(abs(lis$lower - lower), 1e-5)
  expect_lte(abs(lis$upper - upper), 1e-5)

  # transform back to distribution
  dis2 <- list_as_distribution(lis)
  expect_equal(class(dis2), class(dis))
  expect_lte(abs(dis2$lower - lower), 1e-5)
  expect_lte(abs(dis2$upper - upper), 1e-5)
})

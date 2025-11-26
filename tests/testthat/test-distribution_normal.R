context("test-distribution_normal")

test_that("normal_distribution works with fixed values", {
  dis <- normal_distribution(mean = 50, sd = 10)
  expect_match(as.character(dis), "N\\(50[\\.0]*, 10[\\.0]*\\)")

  dis <- normal_distribution(mean = 5, sd = 1, lower = -Inf, upper = Inf)
  dis <- normal_distribution(mean = 5, sd = 1, lower = 0, upper = Inf)
  dis <- normal_distribution(mean = 5, sd = 1, lower = -Inf, upper = 0)
})

test_that("normal_distribution works with random values", {
  mean <- runif(1, 50, 100)
  sd <- runif(1, 5, 10)

  # create distribution
  dis <- normal_distribution(mean = mean, sd = sd)
  expect_equal(dis$mean, mean, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(dis$sd, sd, tolerance = 1e-4, check.environment = FALSE)

  # transform to list
  lis <- as.list(dis)
  expect_equal(lis$type, gsub("_distribution", "", class(dis))[[1]], check.environment = FALSE)
  expect_equal(lis$mean, mean, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(lis$sd, sd, tolerance = 1e-4, check.environment = FALSE)

  # transform back to distribution
  dis2 <- as_distribution(lis)
  expect_equal(class(dis2), class(dis), check.environment = FALSE)
  expect_equal(dis2$mean, mean, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(dis2$sd, sd, tolerance = 1e-4, check.environment = FALSE)

  # test dist and quan functions
  dfun <- distribution_function(dis)
  qfun <- quantile_function(dis)

  expect_equal(qfun(0), -Inf, check.environment = FALSE)
  expect_equal(qfun(0.5), mean, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(qfun(1), Inf, check.environment = FALSE)
  expect_equal(dfun(c(-Inf, mean, Inf)), c(0, .5, 1), tolerance = 1e-4, check.environment = FALSE)
})

test_that("normal_distribution with limits works with random values", {
  mean <- runif(1, 50, 100)
  sd <- runif(1, 5, 10)
  lower <- runif(1, 0, 49)
  upper <- runif(1, 101, 200)

  # create distribution
  dis <- normal_distribution(mean = mean, sd = sd, lower = lower, upper = upper)
  expect_equal(dis$lower, lower, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(dis$upper, upper, tolerance = 1e-4, check.environment = FALSE)

  # transform to list
  lis <- as.list(dis)
  expect_equal(lis$type, gsub("_distribution", "", class(dis))[[1]], check.environment = FALSE)
  expect_equal(lis$lower, lower, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(lis$upper, upper, tolerance = 1e-4, check.environment = FALSE)

  # transform back to distribution
  dis2 <- as_distribution(lis)
  expect_equal(class(dis2), class(dis), check.environment = FALSE)
  expect_equal(dis2$lower, lower, tolerance = 1e-4, check.environment = FALSE)
  expect_equal(dis2$upper, upper, tolerance = 1e-4, check.environment = FALSE)
})

test_that("normal_distribution errors when expected", {
  expect_error(normal_distribution(mean = -Inf, sd = 0), "mean is not bounded by \\(-Inf,Inf\\)")
  expect_error(normal_distribution(mean = NA, sd = NA), "is not a single numeric value")
  expect_error(normal_distribution(mean = 0, sd = Inf), "sd is not bounded by \\(-Inf,Inf\\)")

  expect_error(normal_distribution(mean = 1, sd = 1, lower = Inf, upper = 0), "lower is not bounded by \\[-Inf,Inf\\)")
  expect_error(normal_distribution(mean = 1, sd = 1, lower = NA, upper = NA), "is not a single numeric value")
  expect_error(normal_distribution(mean = 1, sd = 1, lower = 0, upper = -Inf), "upper is not bounded by \\(-Inf,Inf\\]")
  expect_error(normal_distribution(mean = 1, sd = 1, lower = 10, upper = 0), "lower not less than upper")
})

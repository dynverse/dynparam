context("test-distribution_uniform")

test_that("uniform_distribution works with fixed values", {
  dis <- uniform_distribution(lower = 0, upper = 100)
  expect_match(as.character(dis), "U\\(0[\\.0]*, 100[\\.0]*\\)")
})

test_that("uniform_distribution works with random values", {
  lower <- runif(1, 1, 5)
  upper <- runif(1, 6, 10)
  middle <- c(lower, upper) %>% mean()

  # create distribution
  dis <- uniform_distribution(lower = lower, upper = upper)
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

  # test dist and quan functions
  dfun <- distribution_function(dis)
  qfun <- quantile_function(dis)

  expect_equal(qfun(c(0, 0.5, 1)), c(lower, middle, upper), tolerance = 1e-4, check.environment = FALSE)
  expect_equal(dfun(c(lower, middle, upper)), c(0, .5, 1), tolerance = 1e-4, check.environment = FALSE)
})

test_that("uniform_distribution errors when expected", {
  expect_error(uniform_distribution(lower = -Inf, upper = 0), "lower is not bounded by \\(-Inf,Inf\\)")
  expect_error(uniform_distribution(lower = NA, upper = NA), "is not a single numeric value")
  expect_error(uniform_distribution(lower = ~help_us, upper = "we need more wood"), "is not a single numeric value")
  expect_error(uniform_distribution(lower = 0, upper = Inf), "upper is not bounded by \\(-Inf,Inf\\)")
  expect_error(uniform_distribution(lower = 10, upper = 0), "lower not less than upper")
})

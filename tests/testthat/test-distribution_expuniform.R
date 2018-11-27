context("test-distribution_expuniform")

test_that("expuniform_distribution works with fixed values", {
  dis <- expuniform_distribution(lower = exp(0), upper = exp(100))
  expect_match(as.character(dis), "e\\^U\\(0[\\.0]*, 100[\\.0]*\\)")
})

test_that("expuniform_distribution works with random values", {
  lower <- 10^runif(1, 1, 5)
  upper <- 10^runif(1, 6, 10)
  middle <- c(lower, upper) %>% log() %>% mean() %>% exp()

  # create distribution
  dis <- expuniform_distribution(lower = lower, upper = upper)
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

  # test dist and quan functions
  dfun <- distribution_function(dis)
  qfun <- quantile_function(dis)

  expect_true(all(abs(qfun(c(0, 0.5, 1)) - c(lower, middle, upper)) < 1e-5))
  expect_true(all(abs(dfun(c(lower, middle, upper)) - c(0, .5, 1)) < 1e-5))
})

test_that("expuniform_distribution errors when expected", {
  expect_error(expuniform_distribution(lower = -Inf, upper = 0), "lower.*should be finite")
  expect_error(expuniform_distribution(lower = NA, upper = NA), "lower.*should be finite")
  expect_error(expuniform_distribution(lower = ~help_us, upper = "we need more wood"), "should be finite")
  expect_error(expuniform_distribution(lower = 0, upper = Inf), "upper.*should be finite")
  expect_error(expuniform_distribution(lower = 10, upper = 0), "lower.*should not be greater than.*upper")
})

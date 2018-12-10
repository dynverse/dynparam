context("test-set")

test_that("parameter set test test", {
  num_iter_p <- integer_parameter(
    id = "num_iter",
    default = 100L,
    distribution = expuniform_distribution(lower = 1L, upper = 10000L),
    description = "Number of iterations"
  )
  delta_p <- numeric_parameter(
    id = "delta",
    default = c(4.5, 2.4, 1.9),
    distribution = normal_distribution(mean = 5, sd = 1),
    description = "Multiplying factors"
  )
  method_p <- character_parameter(
    id = "method",
    default = "kendall",
    values = c("kendall", "spearman", "pearson"),
    description = "Correlation method"
  )
  inverse_p <- logical_parameter(
    id = "inverse",
    default = TRUE,
    description = "Inversion parameter"
  )
  dimred_p <- subset_parameter(
    id = "dimreds",
    default = c("pca", "mds"),
    values = c("pca", "mds", "tsne", "umap", "ica"),
    description = "Which dimensionality reduction methods to apply (can be multiple)"
  )
  ks_p <- range_parameter(
    id = "ks",
    default = c(3L, 15L),
    lower_distribution = uniform_distribution(1L, 5L),
    upper_distribution = uniform_distribution(10L, 20L),
    description = "The numbers of clusters to be evaluated",
    as_integer = TRUE
  )
  quantiles_p <- range_parameter(
    id = "quantiles",
    default = c(0.15, 0.90),
    lower_distribution = uniform_distribution(0, .4),
    upper_distribution = uniform_distribution(.6, 1),
    description = "Quantile cutoff range",
    as_integer = FALSE
  )
  parameters <- parameter_set(
    num_iter_p,
    delta_p,
    method_p,
    inverse_p,
    dimred_p,
    ks_p,
    quantiles_p,
    forbidden = "inverse == (method == 'kendall')"
  )

  expect_is(parameters, "parameter_set")
  expect_equal(parameters$forbidden, "inverse == (method == 'kendall')")

  expect_true(is_parameter_set(parameters))
  expect_true(all(map_lgl(parameters$parameters, is_parameter)))

  expect_equal(parameters$parameters$num_iter, num_iter_p)
  expect_equal(parameters$parameters$delta, delta_p)
  expect_equal(parameters$parameters$method, method_p)
  expect_equal(parameters$parameters$inverse, inverse_p)
  expect_equal(parameters$parameters$dimred, dimred_p)
  expect_equal(parameters$parameters$ks, ks_p)
  expect_equal(parameters$parameters$quantiles, quantiles_p)

  li <- as.list(parameters)

  ps <- as_parameter_set(li)

  expect_equal(parameters, ps)
})

test_that("wrong parse fails gracefully", {
  expect_error(parameter_set(a = 1), "parameter 1 is not a parameter")
  expect_error(parameter_set(logical_parameter(id = "a", default = TRUE), a = 1), "parameter 2 is not a parameter")
  expect_error(parameter_set(list(a = 1)), "parameter 1 is not a parameter")
  expect_error(parameter_set(forbidden = 10), "forbidden is not NULL or forbidden is not a character vector")
})


#' Define a numeric parameter
#'
#' @inheritParams parameter
#'
#' @param distribution A distribution from which the parameter can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#'
#' @export
#'
#' @examples
#' numeric_parameter(
#'   id = "alpha",
#'   default = 0.5,
#'   distribution = uniform_distribution(0.0, 1.0),
#'   description = "Weighting parameter for distance function."
#' )
#'
#' numeric_parameter(
#'   id = "beta",
#'   default = 0.001,
#'   distribution = expuniform_distribution(1e-4, 1e-1),
#'   description = "Percentage decrease in age per iteration"
#' )
numeric_parameter <- function(
  id,
  default,
  distribution,
  description = NULL,
  tuneable = TRUE
) {
  assert_that(is.numeric(default), is_distribution(distribution))

  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description,
    tuneable = tuneable
  ) %>%
    add_class("numeric_parameter")
}

#' @export
as_paramhelper.numeric_parameter <- function(x) {
  dfun <- distribution_function(x$distribution)
  qfun <- quantile_function(x$distribution)
  length <- length(x$default)

  requireNamespace("ParamHelpers")
  fun <- if (length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = x$id,
    lower = dfun(x$distribution$lower),
    upper = dfun(x$distribution$upper),
    default = dfun(x$default),
    trafo = qfun,
    tunable = x$tuneable
  )
  if (length != 1) args$len <- length

  do.call(fun, args)
}

#' @export
#' @rdname super
as_descriptive_tibble.numeric_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "numeric",
    domain = as.character(x$distribution),
    default = collapse_set(x$default)
  )
}

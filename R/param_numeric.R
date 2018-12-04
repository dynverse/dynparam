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
  description = NULL
) {
  assert_that(is.numeric(default), is_distribution(distribution))

  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description
  ) %>%
    add_class("numeric_parameter")
}

#' @export
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.numeric_parameter <- function(param) {
  dfun <- distribution_function(param$distribution)
  qfun <- quantile_function(param$distribution)
  length <- length(param$default)

  fun <- if (length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = dfun(param$distribution$lower),
    upper = dfun(param$distribution$upper),
    default = dfun(param$default),
    trafo = qfun
  )
  if (length != 1) args$len <- length

  do.call(fun, args)
}

#' @export
as.character.numeric_parameter <- function(x, ...) {
  subset_char <- if (length(x$default) == 1) " \u2208 " else " \u2286 "
  paste0("[numeric] ", x$id, subset_char, as.character(x$distribution), ", default=", collapse_set(x$default))
}

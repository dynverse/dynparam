#' Define a numeric parameter
#'
#' @inheritParams parameter
#'
#' @param length The length of the vector of this parameter (default 1).
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
  length = 1
) {
  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description,
    length = length
  ) %>%
    add_class("numeric_parameter")
}

#' @export
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.numeric_parameter <- function(param) {
  dfun <- distribution_function(param$distribution)
  qfun <- quantile_function(param$distribution)

  fun <- if (param$length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = dfun(param$distribution$lower),
    upper = dfun(param$distribution$upper),
    default = dfun(param$default),
    trafo = qfun
  )
  if (param$length != 1) args$len <- param$length
  list(params = do.call(fun, args))
}

#' @export
as_list.numeric_parameter <- function(x) {
  list(
    class = "numeric_parameter",
    id = x$id,
    default = x$default,
    description = x$description,
    distribution = as_list(x$distribution),
    length = x$length
  )
}

#' @export
as.character.numeric_parameter <- function(x, ...) {
  subset_char <- if (x$length == 1) " \u2208 " else " \u2286 "
  paste0(x$id, subset_char, as.character(x$distribution), ", class=", x$class, ", default=", collapse_vector(x$default))
}

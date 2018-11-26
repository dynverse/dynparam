#' Define a integer parameter
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
#' integer_parameter(
#'   id = "k",
#'   default = 5,
#'   distribution = uniform_distribution(3, 10),
#'   description = "The number of clusters."
#' )
#'
#' integer_parameter(
#'   id = "num_iter",
#'   default = 100,
#'   distribution = expuniform_distribution(10, 10000),
#'   description = "The number of iterations."
#' )
integer_parameter <- function(
  id,
  default,
  distribution,
  description = NULL,
  length = 1
) {
  parameter(id = id, default = default, distribution = distribution, description = description, length = length) %>%
    add_class("integer_parameter")
}

#' @export
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.integer_parameter <- function(param) {
  dfun <- distribution_function(param$distribution)
  qfun <- quantile_function(param$distribution)

  fun <- if (param$length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = dfun(param$distribution$lower - .5 + 1e-10),
    upper = dfun(param$distribution$upper + .5 - 1e-10),
    default = dfun(param$default),
    trafo = function(x) round(qfun(x))
  )
  if (param$length != 1) args$len <- param$length
  list(params = list(do.call(fun, args)))
}

#' @export
as_list.integer_parameter <- function(x) {
  lst(
    class = "integer_parameter",
    id = x$id,
    default = x$default,
    distribution = as_list(x$distribution),
    description = x$description,
    length = x$length
  )
}

#' @export
as.character.integer_parameter <- function(x, ...) {
  subset_char <- if (x$length == 1) " \u2208 " else " \u2286 "
  paste0("[integer] ", x$id, subset_char, as.character(x$distribution), ", default=", collapse_vector(x$default))
}

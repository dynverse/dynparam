#' Define a integer parameter
#'
#' @inheritParams parameter
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
  distribution$type <- "integer"
  parameter(id = id, default = default, distribution = distribution, description = description, length = length) %>%
    extend_with("integer_parameter", type = "integer")
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
  do.call(fun, args)
}

#' @export
as_list.integer_parameter <- function(x) {
  lst(
    id = x$id,
    default = x$default,
    distribution = as_list(x$distribution),
    description = x$description,
    length = x$length
  )
}

#' @export
as.character.integer_parameter <- function(x, ...) {
  paste0(x$id, " \u2286 ", as.character(x$distribution), ", type=", x$type, ", default=", collapse_vector(x$default))
}

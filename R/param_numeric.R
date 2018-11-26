#' Define a numeric parameter
#'
#' @inheritParams parameter
#' @param distribution A distribution from which the parameter can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#'
#' @export
#'
#' @examples
#' numeric_parameter(
#'   id = "alpha",
#'   default = 0.5,
#'   distribution = uniform(0.0, 1.0),
#'   description = "Weighting parameter for distance function."
#' )
#'
#' numeric_parameter(
#'   id = "beta",
#'   default = 0.001,
#'   distribution = expuniform(1e-4, 1e-1),
#'   description = "Percentage decrease in age per iteration"
#' )
numeric_parameter <- function(
  id,
  default,
  distribution,
  description = NULL,
  length = 1
) {
  distribution$type <- "numeric"
  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description,
    length = length
  ) %>%
    extend_with("numeric_parameter", type = "numeric")
}

#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.numeric_parameter <- function(param) {
  d2u <- distribution2uniform(param$distribution)
  u2d <- uniform2distribution(param$distribution)

  fun <- if (param$length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = d2u(param$distribution$lower),
    upper = d2u(param$distribution$upper),
    default = d2u(param$default),
    trafo = u2d
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}

as_list.numeric_parameter <- function(param) {
  lst(
    id = param$id,
    default = param$default,
    description = param$description,
    distribution = as_list(li$distribution),
    length = li$length
  )
}

as.character.numeric_parameter <- function(param) {
  paste0(param$id, " \u2282 ", as.character(param$distribution), ", type=", param$type, ", default=", collapse_vector(param$default))
}

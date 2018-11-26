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

#' @S3method as_paramhelper numeric_parameter
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.numeric_parameter <- function(param) {
  dfun <- distribution_function(param$distribution)
  qfun <- quantile_function(param$distribution)

  fun <- if (param$length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = qfun(param$distribution$lower),
    upper = qfun(param$distribution$upper),
    default = qfun(param$default),
    trafo = dfun
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}

#' @S3method as_list numeric_parameter
as_list.numeric_parameter <- function(param) {
  list(
    id = param$id,
    default = param$default,
    description = param$description,
    distribution = as_list(param$distribution),
    length = param$length
  )
}

#' @S3method as.character numeric_parameter
as.character.numeric_parameter <- function(param) {
  paste0(param$id, " \u2282 ", as.character(param$distribution), ", type=", param$type, ", default=", collapse_vector(param$default))
}

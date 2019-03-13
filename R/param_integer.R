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
    add_class("integer_parameter")
}

#' @export
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.integer_parameter <- function(x) {
  dfun <- distribution_function(x$distribution)
  qfun <- quantile_function(x$distribution)
  length <- length(x$default)

  fun <- if (length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = x$id,
    lower = dfun(x$distribution$lower - .5 + 1e-10),
    upper = dfun(x$distribution$upper + .5 - 1e-10),
    default = dfun(x$default),
    trafo = function(x) as.integer(round(qfun(x))),
    tunable = x$tuneable
  )
  if (length != 1) args$len <- length

  do.call(fun, args)
}

#' @export
#' @rdname super
as_descriptive_tibble.integer_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "integer",
    domain = as.character(x$distribution),
    default = collapse_set(x$default)
  )
}

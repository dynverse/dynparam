#' Define a range parameter
#'
#' @inheritParams parameter
#' @param lower_distribution A distribution from which the lower value of the range can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#' @param upper_distribution A distribution from which the upper value fo the range can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#' @param as_integer Whether or not this parameter should be interpreted as an integer.
#'
#' @export
#'
#' @examples
#' range_parameter(
#'   id = "ks",
#'   as_integer = TRUE,
#'   default = c(3L, 15L),
#'   lower_distribution = uniform_distribution(1L, 5L),
#'   upper_distribution = uniform_distribution(10L, 20L),
#'   description = "The numbers of clusters to be evaluated."
#' )
#'
#' range_parameter(
#'   id = "quantiles",
#'   as_integer = FALSE,
#'   default = c(0.1, 0.99),
#'   lower_distribution = uniform_distribution(0, 0.25),
#'   upper_distribution = uniform_distribution(0.9, 1),
#'   description = "The lower and upper quantile thresholds."
#' )
range_parameter <- function(
  id,
  default,
  lower_distribution,
  upper_distribution,
  as_integer = TRUE,
  description = NULL
) {
  assert_that(is.numeric(default), is_distribution(lower_distribution), is_distribution(upper_distribution))

  parameter(
    id = id,
    as_integer = as_integer,
    default = default,
    lower_distribution = lower_distribution,
    upper_distribution = upper_distribution,
    description = description
  ) %>%
    add_class("range_parameter")
}

#' @export
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
#' @importFrom glue glue
#' @importFrom carrier crate
#' @importFrom stats as.formula
as_paramhelper.range_parameter <- function(param) {
  dfun_lower <- distribution_function(param$lower_distribution)
  qfun_lower <- quantile_function(param$lower_distribution)
  dfun_upper <- distribution_function(param$upper_distribution)
  qfun_upper <- quantile_function(param$upper_distribution)

  if (param$as_integer) {
    param$lower_distribution$lower <- param$lower_distribution$lower - .5 + 1e-10
    param$lower_distribution$upper <- param$lower_distribution$upper + .5 - 1e-10
    param$upper_distribution$lower <- param$upper_distribution$lower - .5 + 1e-10
    param$upper_distribution$upper <- param$upper_distribution$upper + .5 - 1e-10

    qfun_lower <- function(x) round(qfun_lower(x))
    qfun_upper <- function(x) round(qfun_upper(x))
  }

  param <-
    ParamHelpers::makeNumericVectorParam(
      id = param$id,
      lower = c(
        dfun_lower(param$lower_distribution$lower),
        dfun_upper(param$upper_distribution$lower)
      ),
      upper = c(
        dfun_lower(param$lower_distribution$upper),
        dfun_upper(param$upper_distribution$upper)
      ),
      len = 2,
      default = c(
        dfun_lower(param$default[[1]]),
        dfun_upper(param$default[[2]])
      ),
      trafo = carrier::crate(
        function(x) {
          c(
            qfun_lower(x[[1]]),
            qfun_upper(x[[2]])
          )
        },
        qfun_lower = qfun_lower,
        qfun_upper = qfun_upper
      )
    )

  forbidden <-
    paste0(param$id, "[1] > ", param$id, "[2]")

  attr(param, "forbidden") <- forbidden

  param
}

#' @export
as.character.range_parameter <- function(x, ...) {
  paste0("[range] ", x$id, " \u2208 ( ", as.character(x$lower_distribution), ", ", as.character(x$upper_distribution), " ), default=(", x$default[[1]], ", ", x$default[[2]], ")")
}

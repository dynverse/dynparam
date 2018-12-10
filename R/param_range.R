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
as_paramhelper.range_parameter <- function(x) {
  dfun_lower <- distribution_function(x$lower_distribution)
  qfun_lower <- quantile_function(x$lower_distribution)
  dfun_upper <- distribution_function(x$upper_distribution)
  qfun_upper <- quantile_function(x$upper_distribution)

  if (x$as_integer) {
    x$lower_distribution$lower <- x$lower_distribution$lower - .5 + 1e-10
    x$lower_distribution$upper <- x$lower_distribution$upper + .5 - 1e-10
    x$upper_distribution$lower <- x$upper_distribution$lower - .5 + 1e-10
    x$upper_distribution$upper <- x$upper_distribution$upper + .5 - 1e-10

    qfun_lower <- carrier::crate(function(y) round(qfun_lower(y)), qfun_lower = qfun_lower)
    qfun_upper <- carrier::crate(function(y) round(qfun_upper(y)), qfun_upper = qfun_upper)
  }

  param <-
    ParamHelpers::makeNumericVectorParam(
      id = x$id,
      lower = c(
        dfun_lower(x$lower_distribution$lower),
        dfun_upper(x$upper_distribution$lower)
      ),
      upper = c(
        dfun_lower(x$lower_distribution$upper),
        dfun_upper(x$upper_distribution$upper)
      ),
      len = 2,
      default = c(
        dfun_lower(x$default[[1]]),
        dfun_upper(x$default[[2]])
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
    paste0(x$id, "[1] > ", x$id, "[2]")

  attr(param, "forbidden") <- forbidden

  param
}

as_character_tibble.range_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "range",
    domain = paste0("( ", as.character(x$lower_distribution), ", ", as.character(x$upper_distribution), " )"),
    default = paste0("(", x$default[[1]], ", ", x$default[[2]], ")")
  )
}

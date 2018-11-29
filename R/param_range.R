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

  params <- list(
    ParamHelpers::makeNumericParam(
      id = paste0(param$id, "_lower"),
      lower = dfun_lower(param$lower_distribution$lower),
      upper = dfun_lower(param$lower_distribution$upper),
      default = dfun_lower(param$default[[1]]),
      trafo = qfun_lower
    ),
    ParamHelpers::makeNumericParam(
      id = paste0(param$id, "_upper"),
      lower = dfun_upper(param$upper_distribution$lower),
      upper = dfun_upper(param$upper_distribution$upper),
      default = dfun_upper(param$default[[2]]),
      trafo = qfun_upper,
      requires = stats::as.formula(glue::glue("~ {param$id}_lower < {param$id}_upper"))
    )
  )

  trafo_fun <-
    carrier::crate(function(df) {
      id <- param$id
      lid <- paste0(param$id, "_lower")
      uid <- paste0(param$id, "_upper")

      df[[id]] <- c(df[[lid]], df[[uid]])
      df[[lid]] <- NULL
      df[[uid]] <- NULL

      df
    }, param = param)

  list(
    params = params,
    trafo_fun = trafo_fun
  )
}

#' @export
as.character.range_parameter <- function(x, ...) {
  paste0("[range] ", x$id, " \u2208 ( ", as.character(x$lower_distribution), ", ", as.character(x$upper_distribution), " ), default=(", x$default[[1]], ", ", x$default[[2]], ")")
}

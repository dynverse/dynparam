#' Define a range parameter
#'
#' @inheritParams parameter
#' @param lower_distribution A distribution from which the lower value of the range can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#' @param upper_distribution A distribution from which the upper value fo the range can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#'
#' @export
#'
#' @examples
#' range_parameter(
#'   id = "ks",
#'   default = c(3, 15),
#'   lower_distribution = uniform_distribution(1, 5),
#'   upper_distribution = uniform_distribution(10, 20),
#'   description = "The numbers of clusters to be evaluated."
#' )
range_parameter <- function(
  id,
  default,
  lower_distribution,
  upper_distribution,
  description = NULL
) {
  assert_that(is.numeric(default), is_distribution(lower_distribution), is_distribution(upper_distribution))

  parameter(
    id = id,
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
as_paramhelper.range_parameter <- function(param) {
  dfun_upper <- distribution_function(param$upper_distribution)
  qfun_upper <- quantile_function(param$upper_distribution)
  dfun_lower <- distribution_function(param$lower_distribution)
  qfun_lower <- quantile_function(param$lower_distribution)

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
      upper = dfun_upper(param$upper_distribution$upper),
      upper = dfun_upper(param$upper_distribution$upper),
      default = dfun_upper(param$default[[2]]),
      trafo = qfun_upper
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
    forbidden = glue::glue("{param$id}_lower > {param$id}_upper")
  )
}

#' @export
as.character.range_parameter <- function(x, ...) {
  paste0("[range] ", x$id, " \u2208 ( ", as.character(x$lower_distribution), ", ", as.character(x$upper_distribution), " ), default=(", x$default[[1]], ",", x$default[[2]], ")")
}

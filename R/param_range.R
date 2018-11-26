#' Define a range parameter
#'
#' @inheritParams parameter
#' @param lower_distribution A distribution from which the lower value of the range can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#' @param upper_distribution A distribution from which the upper value fo the range can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#' @param lower_default The default lower value of the range.
#' @param upper_default The default upper value of the range.
#'
#' @export
#'
#' @examples
#' range_parameter(
#'   id = "ks",
#'   lower_default = 3,
#'   upper_default = 15,
#'   lower_distribution = uniform_distribution(1, 5),
#'   upper_distribution = uniform_distribution(10, 20),
#'   description = "The numbers of clusters to be evaluated."
#' )
range_parameter <- function(
  id,
  lower_default,
  upper_default,
  lower_distribution,
  upper_distribution,
  description = NULL
) {
  lower_distribution$type <- "integer"
  upper_distribution$type <- "integer"
  parameter(
    id = id,
    lower_default = lower_default,
    upper_default = upper_default,
    lower_distribution = lower_distribution,
    upper_distribution = upper_distribution,
    description = description
  ) %>%
    extend_with("range_parameter", type = "range")
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
      default = dfun_lower(param$lower_default),
      trafo = qfun_lower
    ),
    ParamHelpers::makeNumericParam(
      id = paste0(param$id, "_upper"),
      upper = dfun_upper(param$upper_distribution$upper),
      upper = dfun_upper(param$upper_distribution$upper),
      default = dfun_upper(param$upper_default),
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
as_list.range_parameter <- function(x) {
  list(
    id = x$id,
    lower_default = x$lower_default,
    upper_default = x$upper_default,
    description = x$description,
    lower_distribution = as_list(x$lower_distribution),
    upper_distribution = as_list(x$upper_distribution)
  )
}

#' @export
as.character.range_parameter <- function(x, ...) {
  paste0(x$id, " \u2208 ( ", as.character(x$lower_distribution), ", ", as.character(x$upper_distribution), " ), type=", x$type, ", default=(", x$lower_default, ",", x$upper_default, ")")
}

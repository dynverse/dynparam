#' Define a range parameter
#'
#' @inheritParams parameter
#' @param lower_distribution A distribution from which the lower value of the range can be sampled.
#' @param upper_distribution A distribution from which the upper value fo the range can be sampled.
range_parameter <- function(
  id,
  default,
  lower_distribution,
  upper_distribution,
  description = NULL,
  tuneable = TRUE
) {
  assert_that(is.numeric(default), is_distribution(lower_distribution), is_distribution(upper_distribution))

  parameter(
    id = id,
    default = default,
    lower_distribution = lower_distribution,
    upper_distribution = upper_distribution,
    description = description,
    tuneable = tuneable
  ) %>%
    add_class("range_parameter")
}

#' @export
#' @importFrom carrier crate
#' @importFrom stats as.formula
as_paramhelper.range_parameter <- function(x) {
  dfun_lower <- distribution_function(x$lower_distribution)
  qfun_lower <- quantile_function(x$lower_distribution)
  dfun_upper <- distribution_function(x$upper_distribution)
  qfun_upper <- quantile_function(x$upper_distribution)

  if ("integer_range_parameter" %in% class(x)) {
    x$lower_distribution$lower <- x$lower_distribution$lower - .5 + 1e-10
    x$lower_distribution$upper <- x$lower_distribution$upper + .5 - 1e-10
    x$upper_distribution$lower <- x$upper_distribution$lower - .5 + 1e-10
    x$upper_distribution$upper <- x$upper_distribution$upper + .5 - 1e-10

    qfun_lower <- carrier::crate(function(y) round(qfun_lower(y)), qfun_lower = qfun_lower)
    qfun_upper <- carrier::crate(function(y) round(qfun_upper(y)), qfun_upper = qfun_upper)
  }

  requireNamespace("ParamHelpers")
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
      ),
      tunable = x$tuneable
    )

  forbidden <-
    paste0(x$id, "[1] > ", x$id, "[2]")

  attr(param, "forbidden") <- forbidden

  param
}

#' @export
as_descriptive_tibble.range_parameter <- function(x) {
  tibble(
    id = x$id,
    type = ifelse("integer_range_parameter" %in% class(x)[[1]], "integer_range", "numeric_range"),
    domain = paste0("( ", as.character(x$lower_distribution), ", ", as.character(x$upper_distribution), " )"),
    default = paste0("(", x$default[[1]], ", ", x$default[[2]], ")")
  )
}

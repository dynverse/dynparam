#' Define a numeric range parameter
#'
#' @inheritParams range_parameter
#'
#' @export
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @examples
#' numeric_range_parameter(
#'   id = "quantiles",
#'   default = c(0.1, 0.99),
#'   lower_distribution = uniform_distribution(0, 0.25),
#'   upper_distribution = uniform_distribution(0.9, 1),
#'   description = "The lower and upper quantile thresholds."
#' )
numeric_range_parameter <- function(
  id,
  default,
  lower_distribution,
  upper_distribution,
  description = NULL,
  tuneable = TRUE
) {
  assert_that(is.numeric(default), is_distribution(lower_distribution), is_distribution(upper_distribution))

  range_parameter(
    id = id,
    default = default,
    lower_distribution = lower_distribution,
    upper_distribution = upper_distribution,
    description = description,
    tuneable = tuneable
  ) %>%
    add_class("numeric_range_parameter")
}

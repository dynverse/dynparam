#' Define a integer range parameter
#'
#' @inheritParams range_parameter
#'
#' @export
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @examples
#' integer_range_parameter(
#'   id = "ks",
#'   default = c(3L, 15L),
#'   lower_distribution = uniform_distribution(1L, 5L),
#'   upper_distribution = uniform_distribution(10L, 20L),
#'   description = "The numbers of clusters to be evaluated."
#' )
integer_range_parameter <- function(
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
    add_class("integer_range_parameter")
}

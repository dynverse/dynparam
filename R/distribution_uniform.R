#' Uniform distribution
#'
#' Distributions are used for defining the domain of an [integer_parameter()] or [numeric_parameter()].
#'
#' @inheritParams distribution
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @export
#'
#' @examples
#' uniform_distribution(1, 10)
uniform_distribution <- function(lower, upper) {
  assert_that(
    is_single_numeric(lower), is_bounded(lower),
    is_single_numeric(upper), is_bounded(upper)
  )
  distribution(lower, upper) %>%
    add_class("uniform_distribution")
}

#' @export
#' @importFrom stats punif
#' @importFrom carrier crate
distribution_function.uniform_distribution <- function(dist) {
  carrier::crate(
    ~ stats::punif(., min = min, max = max),
    min = dist$lower,
    max = dist$upper
  )
}

#' @export
#' @importFrom stats qunif
#' @importFrom carrier crate
quantile_function.uniform_distribution <- function(dist) {
  carrier::crate(
    ~ stats::qunif(., min = min, max = max),
    min = dist$lower,
    max = dist$upper
  )
}

#' @export
as.character.uniform_distribution <- function(x, ...) {
  paste0("U(", x$lower, ", ", x$upper, ")")
}

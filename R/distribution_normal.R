#' Normal distribution
#'
#' Distributions are used for defining the domain of an [integer_parameter()] or [numeric_parameter()].
#'
#' @param mean Mean of the distribution
#' @param sd Standard deviation of the distribution.
#' @param lower An optional lower limit.
#' @param upper An optional upper limit.
#'
#' @export
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @examples
#' normal_distribution(mean = 0, sd = 1)
#'
#' normal_distribution(mean = 5, sd = 1, lower = 1, upper = 10)
normal_distribution <- function(mean, sd, lower = -Inf, upper = Inf) {
  assert_that(
    is_single_numeric(mean), is_bounded(mean),
    is_single_numeric(sd), is_bounded(sd),
    is_single_numeric(lower), is_bounded(lower, lower_closed = TRUE),
    is_single_numeric(upper), is_bounded(upper, upper_closed = TRUE)
  )
  distribution(lower = lower, upper = upper, mean, sd) %>%
    add_class("normal_distribution")
}

#' @export
#' @importFrom stats pnorm
#' @importFrom carrier crate
distribution_function.normal_distribution <- function(dist) {
  carrier::crate(
    ~ stats::pnorm(., mean = mean, sd = sd),
    mean = dist$mean,
    sd = dist$sd
  )
}

#' @export
#' @importFrom stats qnorm
#' @importFrom carrier crate
quantile_function.normal_distribution <- function(dist) {
  carrier::crate(
    ~ stats::qnorm(., mean = mean, sd = sd),
    mean = dist$mean,
    sd = dist$sd
  )
}

#' @export
as.character.normal_distribution <- function(x, ...) {
  paste0("N(", x$mean, ", ", x$sd, ")")
}

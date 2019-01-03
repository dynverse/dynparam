#' Exponentially scaled uniform distribution.
#'
#' @inheritParams distribution
#'
#' @export
#'
#' @examples
#' expuniform_distribution(1, 10000)
#'
#' expuniform_distribution(1e-5, 1e-2)
expuniform_distribution <- function(lower, upper) {
  assert_that(
    is_single_numeric(lower), is_bounded(lower),
    is_single_numeric(upper), is_bounded(upper)
  )
  distribution(lower, upper) %>%
    add_class("expuniform_distribution")
}

#' @export
#' @importFrom stats punif
#' @importFrom carrier crate
distribution_function.expuniform_distribution <- function(dist) {
  carrier::crate(
    ~ stats::punif(log(.), min = min, max = max),
    min = log(dist$lower),
    max = log(dist$upper)
  )
}

#' @export
#' @importFrom stats qunif
#' @importFrom carrier crate
quantile_function.expuniform_distribution <- function(dist) {
  carrier::crate(
    ~ exp(stats::qunif(., min = min, max = max)),
    min = log(dist$lower),
    max = log(dist$upper)
  )
}

#' @export
as.character.expuniform_distribution <- function(x, ...) {
  paste0("e^U(", sprintf("%.2f", log(x$lower)), ", ", sprintf("%.2f", log(x$upper)), ")")
}

#' Exponentially scaled uniform distribution.
#'
#' @param lower Lower limit of the distribution.
#' @param upper Upper limit of the distribution.
#'
#' @export
#'
#' @examples
#' expuniform_distribution(1, 10000)
#'
#' expuniform_distribution(1e-5, 1e-2)
expuniform_distribution <- function(lower, upper) {
  if (!check_finite(lower)) {stop("Provide finite lower boundary when using an uniformly distributed parameter")}
  if (!check_finite(upper)) {stop("Provide finite upper boundary when using an uniformly distributed parameter")}

  p <- lst(lower, upper)
  class(p) <- c("expuniform_distribution", "distribution", "list")
  p
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

#' @export
as_list.expuniform_distribution <- function(x) {
  lst(
    class = "expuniform_distribution",
    lower = x$lower,
    upper = x$upper
  )
}

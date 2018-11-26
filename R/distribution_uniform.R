#' Uniform distribution
#'
#' @param lower Lower limit of the distribution.
#' @param upper Upper limit of the distribution.
#'
#' @export
#'
#' @examples
#' uniform_distribution(1, 10)
uniform_distribution <- function(lower, upper) {
  if (!check_finite(lower)) {stop("Provide finite lower boundary when using an uniformly distributed parameter")}
  if (!check_finite(upper)) {stop("Provide finite upper boundary when using an uniformly distributed parameter")}

  p <- lst(lower, upper)
  class(p) <- c("uniform_distribution", "distribution", "list")
  p
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

#' @export
as_list.uniform_distribution <- function(x) {
  lst(
    class = "uniform_distribution",
    lower = x$lower,
    upper = x$upper
  )
}

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
  class(p) <- c("distribution", "uniform_distribution", "list")
  p
}

#' @export
#' @importFrom stats punif
distribution_function.uniform_distribution <- function(dist) {
  function(q) stats::punif(q, min = dist$lower, max = dist$upper)
}

#' @export
#' @importFrom stats qunif
quantile_function.uniform_distribution <- function(dist) {
  function(p) stats::qunif(p, dist$lower, dist$upper)
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

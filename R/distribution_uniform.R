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
  class(p) <- c("distribution", "dist_uniform", "list")
  p
}

#' @S3method distribution_function dist_uniform
#' @importFrom stats punif
distribution_function.dist_uniform <- function(dist) {
  function(q) stats::punif(q, min = dist$lower, max = dist$upper)
}

#' @S3method quantile_function dist_uniform
#' @importFrom stats qunif
quantile_function.dist_uniform <- function(dist) {
  function(p) stats::qunif(p, dist$lower, dist$upper)
}

#' @S3method as.character dist_uniform
as.character.dist_uniform <- function(dist) {
  paste0("U(", dist$lower, ", ", dist$upper, ")")
}

#' @S3method as_list dist_uniform
as_list.dist_uniform <- function(dist) {
  lst(lower = dist$lower, upper = dist$upper)
}

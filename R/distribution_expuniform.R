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
  class(p) <- c("distribution", "dist_expuniform", "list")
  p
}

#' @S3method distribution_function dist_expuniform
#' @importFrom stats punif
distribution_function.dist_expuniform <- function(dist) {
  function(q) stats::punif(log(q), min = log(dist$lower), max = log(dist$upper))
}

#' @S3method quantile_function dist_expuniform
#' @importFrom stats qunif
quantile_function.dist_expuniform <- function(dist) {
  function(p) exp(stats::qunif(p, log(dist$lower), log(dist$upper)))
}

#' @S3method as.character dist_expuniform
as.character.dist_expuniform <- function(dist) {
  paste0("e^U(", sprintf("%.2f", log(dist$lower)), ", ", sprintf("%.2f", log(dist$upper)), ")")
}

#' @S3method as_list dist_expuniform
as_list.dist_expuniform <- function(dist) {
  lst(lower = dist$lower, upper = dist$upper)
}

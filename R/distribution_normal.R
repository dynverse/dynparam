#' Normal distribution
#'
#' @param mean Mean of the distribution
#' @param sd Standard deviation of the distribution.
#' @param lower An optional lower limit.
#' @param upper An optional upper limit.
#'
#' @export
#'
#' @examples
#' normal_distribution(mean = 0, sd = 1)
#'
#' normal_distribution(mean = 5, sd = 1, lower = 1, upper = 10)
normal_distribution <- function(mean, sd, lower = -Inf, upper = Inf) {
  if (!check_finite(mean)) {stop("Provide finite mean when using a normal distributed parameter")}
  if (!check_finite(sd)) {stop("Provide sd when using a normal distributed parameter")}

  p <- list(mean = mean, sd = sd, lower = lower, upper = upper)
  class(p) <- c("distribution", "dist_normal", "list")
  p
}

#' @importFrom stats pnorm
distribution_function.dist_normal <- function(dist) {
  function(q) stats::pnorm(q, mean = dist$mean, sd = dist$sd)
}

#' @importFrom stats qnorm
quantile_function.dist_normal <- function(dist) {
  function(p) stats::qnorm(p, mean = dist$mean, sd = dist$sd)
}

as.character.dist_normal <- function(dist) {
  paste0("N(", dist$mean, ", ", dist$sd, ")")
}

as_list.dist_normal <- function(dist) {
  list(mean = dist$mean, sd = dist$sd, lower = dist$lower, upper = dist$upper)
}

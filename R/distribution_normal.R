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
  assert_that(
    is_single_numeric(mean),
    is_single_numeric(sd),
    is_single_numeric(lower, allow_neg_inf = TRUE),
    is_single_numeric(upper, allow_pos_inf = TRUE)
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

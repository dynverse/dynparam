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
  if (!check_finite(lower)) stop("Parameter ", sQuote("lower"), " should be finite")
  if (!check_finite(upper)) stop("Parameter ", sQuote("upper"), " should be finite")
  if (lower > upper) stop("Parameters: ", sQuote("lower"), " should not be greater than ", sQuote("upper"))

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

list_as_distribution.uniform_distribution <- function(li) {
  if (!all(c("class", "lower", "upper") %in% names(li))) return(NULL)
  if (li$class != "uniform_distribution") return(NULL)

  uniform_distribution(lower = li$lower, upper = li$upper)
}

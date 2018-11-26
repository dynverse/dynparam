check_finite <- function(x) length(x) == 1 && is.finite(x)

#' Helper functions for converting distributions from and to other formats
#'
#' @param dist A distribution object.
#' @param li A list to be converted into a distribution.
#'
#' @export
#' @rdname distributions
#'
#' @seealso [dynparam][dynparam].
distribution_function <- function(dist) {
  UseMethod("distribution_function", dist)
}

#' @export
#' @rdname distributions
quantile_function <- function(dist) {
  UseMethod("quantile_function", dist)
}

#' @export
#' @rdname distributions
list_as_distribution <- function(li) {
  distribution <- li$distribution %||% "uniform"

  if (distribution == "uniform") {
    uniform_distribution(lower = li$lower, upper = li$upper)
  } else if (distribution == "normal") {
    normal_distribution(mean = li$mean, sd = li$sd, lower = li$lower %||% -Inf, upper = li$upper %||% Inf)
  } else if (distribution == "expuniform") {
    expuniform_distribution(lower = li$lower, upper = li$upper)
  } else {
    stop("Unknown distribution list format: ", deparse(li, width.cutoff = 100))
  }
}

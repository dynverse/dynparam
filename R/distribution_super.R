check_finite <- function(x) length(x) == 1 && is.finite(x)

#' @export
#' @rdname parameters
distribution_function <- function(param) {
  UseMethod("distribution_function", param)
}

#' @export
#' @rdname parameters
quantile_function <- function(param) {
  UseMethod("quantile_function", param)
}

#' @export
#' @rdname parameters
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

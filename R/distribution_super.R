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
  obj <-
    list_as_distribution.uniform_distribution(li) %||%
    list_as_distribution.expuniform_distribution(li) %||%
    list_as_distribution.normal_distribution(li)

  if (is.null(obj)) {
    stop("Unknown distribution list format: ", deparse(li, width.cutoff = 100))
  }

  obj
}

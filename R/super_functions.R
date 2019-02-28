#' Common transformation functions for dynparam objects
#'
#' @param x A dynparam object (e.g. distribution, parameter, parameter_set)
#'
#' @export
#' @rdname super
as_paramhelper <- function(x) {
  UseMethod("as_paramhelper")
}

#' @export
#' @rdname super
as_roxygen <- function(x) {
  UseMethod("as_roxygen")
}

#' @export
#' @rdname super
as_descriptive_tibble <- function(x) {
  UseMethod("as_descriptive_tibble")
}

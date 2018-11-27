#' Helper function for checking whether a vector is finite and of length one
#'
#' @param x Vector to check for length 1 finiteness
check_finite <- function(x) {
  length(x) == 1 && is.atomic(x) && is.finite(x)
}

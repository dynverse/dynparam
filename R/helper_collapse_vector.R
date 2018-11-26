#' A helper function for collapsing a vector
#'
#' Will surround the collapsed vector with brackets if it has more than one element.
#'
#' @param vec The vector to collapse.
collapse_vector <- function(vec) {
  if (length(vec) == 1) {
    vec
  } else {
    paste0("{", paste(vec, collapse = ", "), "}")
  }
}

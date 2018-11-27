#' A helper function for collapsing a set
#'
#' Will surround the collapsed set with brackets if it has more than one element.
#'
#' @param ... Characters to collapse
#' @param sep Seperator between elements
#' @param prefix A prefix
#' @param postfix A postfix
collapse_set <- function(..., sep = ", ", prefix = "{", postfix = "}") {
  vec <- unlist(c(...))

  if (length(vec) == 1) {
    as.character(vec)
  } else {
    paste0(prefix, paste(vec, collapse = sep), postfix)
  }
}

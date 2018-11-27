#' Helper function for checking whether a vector is finite and of length one
#'
#' @param x Vector to check for length 1 finiteness
#' @param allow_neg_inf Allow `-Inf``
#' @param allow_pos_inf Allow `Inf``
check_finite <- function(x, allow_neg_inf = FALSE, allow_pos_inf = FALSE) {
  b <- length(x) == 1 && is.numeric(x) && (
    is.finite(x) ||
      (allow_neg_inf && x == -Inf) ||
      (allow_pos_inf && x == Inf)
  )
  b && !is.na(b)
}

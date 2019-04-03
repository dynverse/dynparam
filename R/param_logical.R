#' Define a logical parameter
#'
#' @inheritParams parameter
#'
#' @export
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @examples
#' logical_parameter(
#'   id = "scale",
#'   default = TRUE,
#'   description = "Whether or not to scale the input variables"
#' )
logical_parameter <- function(
  id,
  default,
  description = NULL,
  tuneable = TRUE
) {
  assert_that(is.logical(default))

  parameter(
    id = id,
    default = default,
    description = description,
    tuneable = tuneable
  ) %>%
    add_class("logical_parameter")
}

#' @export
as_paramhelper.logical_parameter <- function(x) {
  length <- length(x$default)

  requireNamespace("ParamHelpers")
  fun <- if (length == 1) ParamHelpers::makeLogicalParam else ParamHelpers::makeLogicalVectorParam

  args <- list(
    id = x$id,
    default = x$default,
    tunable = x$tuneable
  )

  if (length != 1) args$len <- length

  do.call(fun, args)
}

#' @export
as_descriptive_tibble.logical_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "logical",
    default = collapse_set(x$default)
  )
}

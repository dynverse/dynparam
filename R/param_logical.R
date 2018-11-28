#' Define a logical parameter
#'
#' @inheritParams parameter
#'
#' @export
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
  description = NULL
) {
  assert_that(is.logical(default))

  parameter(
    id = id,
    default = default,
    description = description
  ) %>%
    add_class("logical_parameter")
}

#' @export
#' @importFrom ParamHelpers makeLogicalParam makeLogicalVectorParam
as_paramhelper.logical_parameter <- function(param) {
  length <- length(param$default)

  fun <- if (length == 1) ParamHelpers::makeLogicalParam else ParamHelpers::makeLogicalVectorParam

  args <- list(
    id = param$id,
    default = param$default
  )

  if (length != 1) args$len <- length

  list(params = list(do.call(fun, args)))
}

#' @export
as.character.logical_parameter <- function(x, ...) {
  paste0("[logical] ", x$id, ", default=", collapse_set(x$default))
}

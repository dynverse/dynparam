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
  description = NULL,
  length = 1
) {
  parameter(id = id, default = default, distribution = NULL, description = description, length = length) %>%
    extend_with("logical_parameter", type = "logical")
}

#' @export
#' @importFrom ParamHelpers makeLogicalParam makeLogicalVectorParam
as_paramhelper.logical_parameter <- function(param) {
  fun <- if (param$length == 1) ParamHelpers::makeLogicalParam else ParamHelpers::makeLogicalVectorParam
  args <- list(
    id = param$id,
    default = param$default
  )
  if (param$length != 1) args$len <- param$length
  list(params = do.call(fun, args))
}

#' @export
as_list.logical_parameter <- function(x) {
  lst(
    id = x$id,
    default = x$default,
    description = x$description,
    length = x$length
  )
}

#' @export
as.character.logical_parameter <- function(x, ...) {
  paste0(x$id, ", type=", x$type, ", default=", collapse_vector(x$default))
}

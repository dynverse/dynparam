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

#' @importFrom ParamHelpers makeLogicalParam makeLogicalVectorParam
as_paramhelper.logical_parameter <- function(param) {
  fun <- if (param$length == 1) ParamHelpers::makeLogicalParam else ParamHelpers::makeLogicalVectorParam
  args <- list(
    id = param$id,
    default = param$default
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}

as_list.logical_parameter <- function(param) {
  lst(
    id = param$id,
    default = param$default,
    description = param$description,
    length = param$length
  )
}

as.character.logical_parameter <- function(param) {
  paste0(param$id, ", type=", param$type, ", default=", collapse_vector(param$default))
}

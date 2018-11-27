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

  list(params = do.call(fun, args))
}

#' @export
as_list.logical_parameter <- function(x) {
  lst(
    class = "logical_parameter",
    id = x$id,
    default = x$default,
    description = x$description
  )
}

#' @export
as.character.logical_parameter <- function(x, ...) {
  paste0("[logical] ", x$id, ", default=", collapse_set(x$default))
}

list_as_parameter.logical_parameter <- function(li) {
  if (!all(c("class", "id", "default") %in% names(li))) return(NULL)
  if (li$class != "logical_parameter") return(NULL)

  logical_parameter(
    id = li$id,
    default = li$default,
    description = li$description %||% NULL
  )
}

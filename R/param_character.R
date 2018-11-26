#' Define a character / string parameter
#'
#' @inheritParams parameter
#' @param values A set of possible values.
#'
#' @export
#'
#' @examples
#' character_parameter(
#'   id = "method",
#'   default = "pearson",
#'   values = c("pearson", "spearman", "kendall"),
#'   description = "Which correlation coefficient to compute."
#' )
character_parameter <- function(
  id,
  default,
  values,
  description = NULL,
  length = 1
) {
  parameter(id = id, default = default, values = values, description = description, length = length) %>%
    extend_with("character_parameter", type = "character")
}

#' @importFrom ParamHelpers makeDiscreteParam makeDiscreteVectorParam
as_paramhelper.character_parameter <- function(param) {
  fun <- if (param$length == 1) ParamHelpers::makeDiscreteParam else ParamHelpers::makeDiscreteVectorParam
  args <- list(
    id = param$id,
    values = param$values,
    default = param$default
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}

as_list.character_parameter <- function(param) {
  lst(
    id = param$id,
    default = param$default,
    values = as_list(param$values),
    description = param$description,
    length = param$length
  )
}

as.character.character_parameter <- function(param) {
  paste0(param$id, " \u2282 {", paste(param$values, collapse = ", "), "}, type=", param$type, ", default=", collapse_vector(param$default))
}

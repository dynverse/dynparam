#' Define a character / string parameter
#'
#' @inheritParams parameter
#' @param values A set of possible values.
#' @param length The length of the vector of this parameter (default 1).
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
    add_class("character_parameter")
}

#' @export
#' @importFrom ParamHelpers makeDiscreteParam makeDiscreteVectorParam
as_paramhelper.character_parameter <- function(param) {
  fun <- if (param$length == 1) ParamHelpers::makeDiscreteParam else ParamHelpers::makeDiscreteVectorParam
  args <- list(
    id = param$id,
    values = param$values,
    default = param$default
  )
  if (param$length != 1) args$len <- param$length
  list(params = list(do.call(fun, args)))
}

#' @export
as_list.character_parameter <- function(x) {
  lst(
    class = "character_parameter",
    id = x$id,
    default = x$default,
    values = x$values,
    description = x$description,
    length = x$length
  )
}

#' @export
as.character.character_parameter <- function(x, ...) {
  subset_char <- if (x$length == 1) " \u2208 " else " \u2286 "
  paste0("[character] ", x$id, subset_char, "{", paste(x$values, collapse = ", "), "}, default=", collapse_vector(x$default))
}

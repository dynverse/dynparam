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
  description = NULL
) {
  assert_that(is.character(default), is.character(values), default %allin% values)

  parameter(
    id = id,
    default = default,
    values = values,
    description = description
  ) %>%
    add_class("character_parameter")
}

#' @export
#' @importFrom ParamHelpers makeDiscreteParam makeDiscreteVectorParam
as_paramhelper.character_parameter <- function(param) {
  length <- length(param$default)

  fun <- if (length == 1) ParamHelpers::makeDiscreteParam else ParamHelpers::makeDiscreteVectorParam

  args <- list(
    id = param$id,
    values = param$values,
    default = param$default
  )

  if (length != 1) {
    args$len <- length
    args$default <- as.list(args$default)
  }

  list(params = list(do.call(fun, args)))
}

#' @export
as.character.character_parameter <- function(x, ...) {
  subset_char <- if (length(x$default) == 1) " \u2208 " else " \u2286 "
  paste0("[character] ", x$id, subset_char, "{", paste(x$values, collapse = ", "), "}, default=", collapse_set(x$default))
}

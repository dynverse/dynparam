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
  parameter(
    id = id,
    default = default,
    values = unlist(unname(values)),
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

  if (length != 1)
    args$len <- length

  list(params = list(do.call(fun, args)))
}

#' @export
as_list.character_parameter <- function(x) {
  lst(
    class = "character_parameter",
    id = x$id,
    default = x$default,
    values = x$values,
    description = x$description
  )
}

#' @export
as.character.character_parameter <- function(x, ...) {
  subset_char <- if (length(x$default) == 1) " \u2208 " else " \u2286 "
  paste0("[character] ", x$id, subset_char, "{", paste(x$values, collapse = ", "), "}, default=", collapse_set(x$default))
}

list_as_parameter.character_parameter <- function(li) {
  if (!all(c("class", "id", "default", "values") %in% names(li))) return(NULL)
  if (li$class != "character_parameter") return(NULL)

  character_parameter(
    id = li$id,
    default = li$default,
    values = li$values,
    description = li$description %||% NULL
  )
}

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
as_paramhelper.character_parameter <- function(x) {
  length <- length(x$default)

  fun <- if (length == 1) ParamHelpers::makeDiscreteParam else ParamHelpers::makeDiscreteVectorParam

  args <- list(
    id = x$id,
    values = x$values,
    default = x$default
  )

  if (length != 1) {
    args$len <- length
    args$default <- as.list(args$default)
    args$values <- as.list(args$values)
  }

  do.call(fun, args)
}

as_character_tibble.character_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "character",
    domain = paste0("{", paste(x$values, collapse = ", "), "}"),
    default = collapse_set(x$default)
  )
}

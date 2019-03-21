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
  tuneable = TRUE
) {
  assert_that(is.character(default), is.character(values), default %all_in% values)

  parameter(
    id = id,
    default = default,
    values = values,
    description = description,
    tuneable = tuneable
  ) %>%
    add_class("character_parameter")
}

#' @export
as_paramhelper.character_parameter <- function(x) {
  length <- length(x$default)

  requireNamespace("ParamHelpers")
  fun <- if (length == 1) ParamHelpers::makeDiscreteParam else ParamHelpers::makeDiscreteVectorParam

  args <- list(
    id = x$id,
    values = x$values,
    default = x$default,
    tunable = x$tuneable
  )

  if (length != 1) {
    args$len <- length
    args$default <- as.list(args$default)
    args$values <- as.list(args$values)
  }

  do.call(fun, args)
}

#' @export
#' @rdname super
as_descriptive_tibble.character_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "character",
    domain = paste0("{", paste(x$values, collapse = ", "), "}"),
    default = collapse_set(x$default)
  )
}

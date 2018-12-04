#' Define a subset parameter
#'
#' @inheritParams parameter
#' @param values A set of possible values.
#'
#' @export
#'
#' @examples
#' subset_parameter(
#'   id = "dimreds",
#'   default = c("pca", "mds"),
#'   values = c("pca", "mds", "tsne", "umap", "ica"),
#'   description = "Which dimensionality reduction methods to apply (can be multiple)"
#' )
subset_parameter <- function(
  id,
  default,
  values,
  description = NULL
) {
  assert_that(is.character(default), is.character(values), default %allin% values)

  parameter(
    id = id,
    default = default,
    values = unlist(unname(values)),
    description = description
  ) %>%
    add_class("subset_parameter")
}

#' @export
#' @importFrom ParamHelpers makeDiscreteVectorParam
#' @importFrom carrier crate
as_paramhelper.subset_parameter <- function(param) {
  param <-
    ParamHelpers::makeLogicalVectorParam(
      id = param$id,
      default = param$values %in% param$default,
      len = length(param$values)
    )
  trafo_fun <-
    carrier::crate(function(df) {
      df[[param$id]] <- param$values[df[[param$id]]]
      df
    }, param = param)

  list(params = list(param), trafo_fun = trafo_fun)
}

#' @export
as.character.subset_parameter <- function(x, ...) {
  paste0("[subset] ", x$id, " = {x | x \u2286 {", paste(x$values, collapse = ", "), "}}, default=", collapse_set(x$default))
}

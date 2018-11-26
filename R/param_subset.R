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
  parameter(id = id, default = default, values = values, description = description, length = length(values)) %>%
    extend_with("subset_parameter", type = "subset")
}

#' @export
#' @importFrom ParamHelpers makeDiscreteVectorParam
#' @importFrom carrier crate
as_paramhelper.subset_parameter <- function(param) {
  list(
    ParamHelpers::makeDiscreteVectorParam(
      id = param$id,
      values = param$values,
      default = param$default,
      len = param$length
    ),
    carrier::crate(function(df) {
      df %>% mutate_at(param$id, ~ map(., function(x) param$values[x]))
    }, param = param)
  )
}

#' @export
as_list.subset_parameter <- function(x) {
  lst(
    id = x$id,
    default = x$default,
    values = as_list(x$values),
    description = x$description,
    length = x$length
  )
}

#' @export
as.character.subset_parameter <- function(x, ...) {
  paste0(x$id, " = {x | x \u2286 {", paste(x$values, collapse = ", "), "}}, type=", x$type, ", default=", collapse_vector(x$default))
}

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

makeDiscreteVectorParamWithTrafo <- function(
  id, len, values, default, trafo = NULL, requires = NULL,
  tunable = TRUE, special.vals = list()
) {
  ParamHelpers:::makeParam(
    id = id, type = "discretevector", learner.param = FALSE, len = len,
    values = values, default = default,
    trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals
  )
}

#' @export
#' @importFrom carrier crate
as_paramhelper.subset_parameter <- function(param) {
  makeDiscreteVectorParamWithTrafo(
    id = param$id,
    default = as.list(ifelse(param$values %in% param$default, "TRUE", "FALSE")),
    len = length(param$values),
    values = list("TRUE", "FALSE"),
    trafo = carrier::crate(function(x) values[unlist(x) == "TRUE"], values = param$values)
  )
}

#' @export
as.character.subset_parameter <- function(x, ...) {
  paste0("[subset] ", x$id, " = {x | x \u2286 {", paste(x$values, collapse = ", "), "}}, default=", collapse_set(x$default))
}

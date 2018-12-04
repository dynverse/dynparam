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

make_discrete_vector_param <- function(
  id, len, values, default, requires = NULL,
  tunable = TRUE, special.vals = list(), trafo = NULL
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
  par <- make_discrete_vector_param(
    id = param$id,
    default = ifelse(param$values %in% param$default, "TRUE", "FALSE"),
    len = length(param$values),
    values = c("TRUE", "FALSE"),
    trafo = carrier::crate(function(x) values[x == "TRUE"], values = param$values) # this would be so nice but it doesn't work
  )
}

#' @export
as.character.subset_parameter <- function(x, ...) {
  paste0("[subset] ", x$id, " = {x | x \u2286 {", paste(x$values, collapse = ", "), "}}, default=", collapse_set(x$default))
}

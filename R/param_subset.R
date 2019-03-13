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
  assert_that(is.character(default), is.character(values), default %all_in% values)

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
as_paramhelper.subset_parameter <- function(x) {
  values <- x$values
  makeDiscreteVectorParamWithTrafo(
    id = x$id,
    default = as.list(ifelse(x$values %in% x$default, "TRUE", "FALSE")),
    len = length(x$values),
    values = list("TRUE", "FALSE"),
    trafo = carrier::crate(function(x) values[unlist(x) == "TRUE"], values = values),
    tuneable = x$tuneable
  )
}

#' @export
#' @rdname super
as_descriptive_tibble.subset_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "subset",
    domain = paste0("{x | x \u2286 {", paste(x$values, collapse = ", "), "}}"),
    default = collapse_set(x$default)
  )
}

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
  description = NULL,
  tuneable = TRUE
) {
  assert_that(is.character(default), is.character(values), default %all_in% values)

  parameter(
    id = id,
    default = default,
    values = unlist(unname(values)),
    description = description,
    tuneable = tuneable
  ) %>%
    add_class("subset_parameter")
}

#' @export
#' @importFrom carrier crate
as_paramhelper.subset_parameter <- function(x) {
  values <- x$values
  requireNamespace("ParamHelpers")
  ParamHelpers::makeIntegerVectorParam(
    id = x$id,
    default = as.integer(x$values %in% x$default),
    len = length(x$values),
    lower = 0L,
    upper = 1L,
    trafo = carrier::crate(function(x) values[as.logical(x)], values = values),
    tunable = x$tunable
  )
}

#' @export
#' @rdname super
as_descriptive_tibble.subset_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "subset",
    domain = paste0("all subsets of {", paste(x$values, collapse = ", "), "}"),
    default = collapse_set(x$default)
  )
}

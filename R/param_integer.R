#' Define a integer parameter
#'
#' @inheritParams parameter
#' @param distribution A distribution from which the parameter can be sampled.
#'   See [?dynparam][dynparam::dynparam] for a list of possible distributions.
#'
#' @export
#'
#' @examples
#' integer_parameter(
#'   id = "k",
#'   default = 5,
#'   distribution = uniform_distribution(3, 10),
#'   description = "The number of clusters."
#' )
#'
#' integer_parameter(
#'   id = "num_iter",
#'   default = 100,
#'   distribution = expuniform_distribution(10, 10000),
#'   description = "The number of iterations."
#' )
integer_parameter <- function(
  id,
  default,
  distribution,
  description = NULL
) {
  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description
  ) %>%
    add_class("integer_parameter")
}

#' @export
#' @importFrom ParamHelpers makeNumericParam makeNumericVectorParam
as_paramhelper.integer_parameter <- function(param) {
  dfun <- distribution_function(param$distribution)
  qfun <- quantile_function(param$distribution)
  length <- length(param$default)

  fun <- if (length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = dfun(param$distribution$lower - .5 + 1e-10),
    upper = dfun(param$distribution$upper + .5 - 1e-10),
    default = dfun(param$default),
    trafo = function(x) round(qfun(x))
  )
  if (length != 1) args$len <- length
  list(params = list(do.call(fun, args)))
}

#' @export
as_list.integer_parameter <- function(x) {
  lst(
    class = "integer_parameter",
    id = x$id,
    default = x$default,
    distribution = as_list(x$distribution),
    description = x$description
  )
}

#' @export
as.character.integer_parameter <- function(x, ...) {
  subset_char <- if (length(x$default) == 1) " \u2208 " else " \u2286 "
  paste0("[integer] ", x$id, subset_char, as.character(x$distribution), ", default=", collapse_set(x$default))
}

list_as_parameter.integer_parameter <- function(li) {
  if (!all(c("class", "id", "default", "distribution") %in% names(li))) return(NULL)
  if (li$class != "integer_parameter") return(NULL)

  integer_parameter(
    id = li$id,
    default = li$default,
    distribution = list_as_distribution(li$distribution),
    description = li$description %||% NULL
  )
}

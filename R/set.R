#' Parameter set helper functions
#'
#' @param ... A list of parameters to wrap in a parameter set.
#' @param x An object for which to check whether it is a parameter set.
#' @param param A parameter set to convert to a ParamHelpers object.
#' @param forbidden States forbidden region of parameter via a character vector, which will be turned into an expression.
#'
#' @export
#'
#' @seealso [character_parameter()], [integer_parameter()], [logical_parameter()], [numeric_parameter()], [range_parameter()], [subset_parameter()], [dynparam]
parameter_set <- function(..., forbidden = NULL) {
  parameters <- list(...)

  assert_that(all(map_lgl(parameters, is_parameter)))

  if (!is.null(forbidden)) {
    assert_that(is.character(forbidden))
    attr(parameters, "forbidden") <- forbidden
  }

  parameters %>% add_class("parameter_set")
}

#' @export
#' @rdname parameter_set
is_parameter_set <- function(x) {
  "parameter_set" %in% class(x)
}
on_failure(is_parameter_set) <- function(call, env) {
  paste0(deparse(call$x), " is not a parameter set")
}

#' @export
#' @rdname parameter_set
as_paramhelper.parameter_set <- function(param) {
  assert_that(is_parameter_set(param))

  params <- map(param, as_paramhelper)

  forbiddens <- params %>%
    map(attr, "forbidden") %>%
    c(attr(param, "forbidden")) %>%
    unlist() %>%
    paste("(", ., ")", collapse = " & ", sep = "") %>%
    parse(text = .)

  ParamHelpers::makeParamSet(
    params = params,
    forbidden = forbiddens
  )
}

#' @rdname parameter_set
#' @export
as.list.parameter_set <- function(x, ...) {
  assert_that(is_parameter_set(x))

  # todo
}


#' @export
#' @rdname parameter
as_parameter_set <- function(li) {
  # check that list has a recognised type
  assert_that("list" %in% class(li))

  # todo
}

#' Parameter set helper functions
#'
#' @param ... Parameters to wrap in a parameter set.
#' @param parameters A list of parameters to wrap in a parameter set.
#' @param x An object for which to check whether it is a parameter set.
#' @param forbidden States forbidden region of parameter via a character vector, which will be turned into an expression.
#'
#' @export
#'
#' @seealso [character_parameter()], [integer_parameter()], [logical_parameter()], [numeric_parameter()], [range_parameter()], [subset_parameter()], [dynparam]
parameter_set <- function(..., parameters = NULL, forbidden = NULL) {
  assert_that(is.null(parameters) || is.list(parameters))
  assert_that(is.null(forbidden) || is.character(forbidden))

  parameters <- c(list(...), parameters)

  # make sure that all parameters are indeed parameters
  for (i in seq_along(parameters)) {
    assert_that(is_parameter(parameters[[i]]), msg = paste0("parameter ", i, " is not a parameter"))
  }

  # add parameter names to parameter list
  names(parameters) <- map_chr(parameters, ~.$id)

  list(
    parameters = parameters,
    forbidden = forbidden
  ) %>% add_class("parameter_set")
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
as_paramhelper.parameter_set <- function(x) {
  assert_that(is_parameter_set(x))

  params <- map(x$parameters, as_paramhelper)

  forbiddens <- params %>%
    map(attr, "forbidden") %>%
    c(x$forbidden) %>%
    unlist()

  forbidden_expr <-
    if (length(forbiddens) == 0) {
      NULL
    } else {
      forbidden_text <- paste("(", forbiddens, ")", collapse = " & ", sep = "")
      parse(text = forbidden_text)
    }

  ParamHelpers::makeParamSet(
    params = params,
    forbidden = forbidden_expr
  )
}

#' @rdname parameter_set
#' @export
as.list.parameter_set <- function(x, ...) {
  assert_that(is_parameter_set(x))

  # transform parameters to list
  out <- list()

  # convert parameters to list and save to out
  for (i in seq_along(x$parameters)) {
    if (is_parameter(x$parameters[[i]])) {
      out[[i]] <- as.list(x$parameters[[i]])
    }
  }

  # save forbidden to list
  if (!is.null(x$forbidden)) {
    out[[length(out) + 1]] <- list(forbidden = x$forbidden)
  }

  out
}


#' @export
#' @rdname parameter
as_parameter_set <- function(li) {
  # check that list has a recognised type
  assert_that("list" %all_in% class(li), is.null(names(li)))

  params <- list()
  forbidden <- NULL

  for (i in seq_along(li)) {
    lin <- li[[i]]
    if (is.list(lin) && lin %has_name% "forbidden" && is.character(lin$forbidden)) {
      forbidden <- lin$forbidden
    } else {
      params[[length(params) + 1]] <- as_parameter(lin)
    }
  }

  # call the constructor
  parameter_set(parameters = params, forbidden = forbidden)
}

#' @export
#' @rdname parameter
as.character.parameter_set <- function(x, ...) {
  assert_that(is_parameter_set(x))

  # transform parameters to list
  x$parameters %>%
    map_chr(as.character) %>%
    paste(collapse = "\n")
}

#' @export
#' @rdname parameter
print.parameter_set <- function(x, ...) {
  cat(as.character(x))
}

#' @export
#' @rdname
sip.parameter_set <- function(x, n = 1) {
  param_sips <- map(x$parameters, sip, n = n)

  purrr::transpose(param_sips)
}

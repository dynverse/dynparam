#' Parameter set helper functions
#'
#' @param ... Parameters to wrap in a parameter set.
#' @param parameters A list of parameters to wrap in a parameter set.
#' @param x An object for which to check whether it is a parameter set.
#' @param param A parameter set to convert to a ParamHelpers object.
#' @param forbidden States forbidden region of parameter via a character vector, which will be turned into an expression.
#'
#' @export
#'
#' @seealso [character_parameter()], [integer_parameter()], [logical_parameter()], [numeric_parameter()], [range_parameter()], [subset_parameter()], [dynparam]
parameter_set <- function(..., parameters = NULL, forbidden = NULL) {
  assert_that(is.null(parameters) || is.list(parameters))
  assert_that(is.null(forbidden) || is.character(forbidden))

  parameters <- c(list(...), parameters)

  assert_that(all(map_lgl(parameters, is_parameter)))

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
as_paramhelper.parameter_set <- function(param) {
  assert_that(is_parameter_set(param))

  params <- map(param$parameters, as_paramhelper)

  forbiddens <- param$parameters %>%
    map(attr, "forbidden") %>%
    c(param$forbidden) %>%
    unlist()

  forbidden_expr <-
    if (length(forbiddens) == 0) {
      NULL
    } else {
      forbiddens %>%
        paste("(", ., ")", collapse = " & ", sep = "") %>%
        parse(text = .)
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
  for (n in names(x)) {
    if (is_parameter(x[[n]])) {
      x$parameters[[n]] <- as.list(x$parameters[[n]])
    }
  }

  x
}


#' @export
#' @rdname parameter
as_parameter_set <- function(li) {
  # check that list has a recognised type
  assert_that("list" %in% class(li), li %has_name% "parameters")

  for (i in seq_along(li$parameters)) {
    lin <- li$parameters[[i]]
    assert_that(is.list(lin), lin %has_name% "type", lin$type %iN% names(parameters))
    li$parameters[[i]] <- as_parameter(lin)
  }

  assert_that(is.null(li$forbidden) || is.character(li$forbidden))

  # call the constructor
  do.call(parameter_set, li)
}

as.character.parameter_set <- function(x, ...) {
  assert_that(is_parameter_set(x))

  # transform parameters to list
  x$parameters %>%
    map_chr(as.character) %>%
    paste(collapse = "\n")
}

print.parameter_set <- function(x, ...) {
  cat(as.character(x))
}

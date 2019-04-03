#' Parameter set helper functions
#'
#' @param ... Parameters to wrap in a parameter set.
#' @param parameters A list of parameters to wrap in a parameter set.
#' @param x An object for which to check whether it is a parameter set.
#' @param forbidden States forbidden region of parameter via a character vector, which will be turned into an expression.
#' @param n Number of objects to return.
#' @param as_tibble Whether or not to return as a tibble.
#' @param li A list to be converted into a parameter set.
#'
#' @section Parameter set instatiations:
#' * `get_defaults()`: Get all default parameters.
#' * `sip()`: It's like `sample()`, but for parameter sets.
#' * `as_paramhelper()`: Convert a parameter set to a ParamHelpers object.
#'
#' @section Serialisation:
#' * `as.list()`: Converting a parameter set to a list.
#' * `as_parameter_set()`: Converting a list back to a parameter set.
#' * `is_parameter_set(x)`: Checking whether something is a parameter set.
#'
#' @export
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @examples
#' parameters <- parameter_set(
#'   integer_parameter(
#'     id = "num_iter",
#'     default = 100L,
#'     distribution = expuniform_distribution(lower = 1L, upper = 10000L),
#'     description = "Number of iterations"
#'   ),
#'   subset_parameter(
#'     id = "dimreds",
#'     default = c("pca", "mds"),
#'     values = c("pca", "mds", "tsne", "umap", "ica"),
#'     description = "Which dimensionality reduction methods to apply (can be multiple)"
#'   ),
#'   integer_range_parameter(
#'     id = "ks",
#'     default = c(3L, 15L),
#'     lower_distribution = uniform_distribution(1L, 5L),
#'     upper_distribution = uniform_distribution(10L, 20L),
#'     description = "The numbers of clusters to be evaluated"
#'   )
#' )
#'
#' get_defaults(parameters)
#'
#' sip(parameters, n = 1)
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


###################################
###        SERIALISATION        ###
###################################

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
#' @rdname parameter_set
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


###################################
###        PARAMETER SETS       ###
###################################

#' @rdname parameter_set
#' @export
get_defaults <- function(x) {
  assert_that(is_parameter_set(x))

  x$parameters %>% map("default")
}

#' @rdname parameter_set
#' @export
sip <- function(x, n = 1, as_tibble = TRUE) {
  assert_that(is_parameter_set(x))
  par_set <- as_paramhelper(x)

  requireNamespace("ParamHelpers")
  requireNamespace("lhs")

  out <-
    ParamHelpers::generateDesign(n = n, par.set = par_set) %>%
    ParamHelpers::dfRowsToList(par.set = par_set) %>%
    map(~ ParamHelpers::trafoValue(., par = par_set))

  if (as_tibble) {
    out <- out %>% list_as_tibble()
  }

  out
}



###################################
###        PARAMHELPERS         ###
###################################
#' @export
#' @rdname parameter_set
as_paramhelper <- function(x) {
  UseMethod("as_paramhelper")
}

#' @export
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
      forbidden_text <- paste("(", forbiddens, ")", collapse = " | ", sep = "")
      parse(text = forbidden_text)
    }

  requireNamespace("ParamHelpers")
  ParamHelpers::makeParamSet(
    params = params,
    forbidden = forbidden_expr
  )
}



###################################
###           CONSOLE           ###
###################################
#' @export
as.character.parameter_set <- function(x, ...) {
  assert_that(is_parameter_set(x))

  # transform parameters to list
  x$parameters %>%
    map_chr(as.character) %>%
    paste(collapse = "\n")
}

#' @export
print.parameter_set <- function(x, ...) {
  cat(as.character(x))
}

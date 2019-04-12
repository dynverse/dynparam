#' Defining, serialising and printing parameters
#'
#' Multiple parameters can be combined in a parameter set.
#' The sections below contain information on how to create, serialise
#' and process a parameter.
#'
#' @section Creating a parameter:
#' * [character_parameter()], [integer_parameter()], [logical_parameter()], [numeric_parameter()]: Creating parameters with basic R data types.
#' * [integer_range_parameter()], [numeric_range_parameter()]: Create a discrete or continuous range parameter.
#' * [subset_parameter()]: A parameter containing a subset of a set of values.
#' * [parameter()]: An abstract function to be used by other parameter functions.
#'
#' @section Serialisation:
#' * `as.list(param)`: Converting a parameter to a list.
#' * `as_parameter(li)`: Converting a list back to a parameter.
#' * `is_parameter(x)`: Checking whether something is a parameter.
#' * `as_descriptive_tibble(param)`: Convert to a tibble containing meta information.
#'
#' @param id The name of the parameter.
#' @param default The default value of the parameter.
#' @param description An optional (but recommended) description of the parameter.
#' @param tuneable Whether or not a parameter is tuneable.
#' @param ... Extra fields to be saved in the parameter.
#' @param x An object (parameter or distribution) to be converted.
#' @param li A list to be converted into a parameter.
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @export
#'
#' @examples
#' int_param <- integer_parameter(
#'   id = "num_iter",
#'   default = 100L,
#'   distribution = expuniform_distribution(lower = 1L, upper = 10000L),
#'   description = "Number of iterations"
#' )
#'
#' print(int_param)
#' li <- as.list(int_param)
#' print(as_parameter(li))
#'
#' subset_param <- subset_parameter(
#'   id = "dimreds",
#'   default = c("pca", "mds"),
#'   values = c("pca", "mds", "tsne", "umap", "ica"),
#'   description = "Which dimensionality reduction methods to apply (can be multiple)"
#' )
#'
#' int_range_param <- integer_range_parameter(
#'   id = "ks",
#'   default = c(3L, 15L),
#'   lower_distribution = uniform_distribution(1L, 5L),
#'   upper_distribution = uniform_distribution(10L, 20L),
#'   description = "The numbers of clusters to be evaluated"
#' )
#'
#' parameter_set(
#'   int_param,
#'   subset_param,
#'   int_range_param
#' )
parameter <- function(
  id,
  default,
  ...,
  description = NULL,
  tuneable = TRUE
) {
  assert_that(
    is.character(id),
    is.null(description) || is.character(description),
    is.logical(tuneable)
  )

  param <- list(
    id = id,
    default = default,
    description = description,
    tuneable = tuneable,
    ...
  )
  class(param) <- c("parameter", "list")
  param
}

#' @export
as.character.parameter <- function(x, ...) {
  lis <- as_descriptive_tibble(x) %>% unlist()

  ifelse(names(lis) == "id", lis, paste0(names(lis), "=", lis)) %>% paste0(collapse = " | ")
}

#' @export
print.parameter <- function(x, ...) {
  cat(as.character(x))
}

#' @rdname parameter
#' @export
as.list.parameter <- function(x, ...) {
  assert_that(is_parameter(x))

  x$type <- gsub("_parameter", "", class(x)[[1]])
  class(x) <- "list"

  # transform distributions to list
  for (n in names(x)) {
    if (is_distribution(x[[n]])) {
      x[[n]] <- as.list(x[[n]])
    }
  }

  x
}

#' @export
#' @rdname parameter
as_parameter <- function(li) {
  # check that list has a recognised type
  assert_that("list" %all_in% class(li), li %has_name% "type", li$type %all_in% names(parameters))

  # check that all the required parameters exist
  constructor_fun <- parameters[[li$type]]
  arg_classes <- formals(constructor_fun) %>% as.list() %>% map_chr(class)
  required_args <- arg_classes %>% keep(~ . == "name") %>% names() %>% setdiff("...")
  assert_that(li %has_names% required_args)

  for (n in names(li)) {
    lin <- li[[n]]

    if ("list" %in% class(lin) && lin %has_name% "type" && lin$type %in% names(distributions)) {
      li[[n]] <- as_distribution(li[[n]])
    } else if (all(map_lgl(lin, is.vector)) && length(unique(map_chr(lin, class))) == 1) {
      li[[n]] <- unlist(lin, recursive = FALSE)
    }
  }

  # call the constructor
  do.call(constructor_fun, li[names(li) != "type"])
}

#' @export
#' @rdname parameter
is_parameter <- function(x) {
  "parameter" %in% class(x)
}
on_failure(is_parameter) <- function(call, env) {
  paste0(deparse(call$x), " is not a parameter")
}

#' Get a description of the parameter
#'
#' @param x The parameter
#' @param sep A separator between different fields
#'
#' @importFrom stringr str_replace_all str_replace str_glue_data
#'
#' @export
get_description <- function(
  x,
  sep = ", "
) {
  lis <- as_descriptive_tibble(x) %>% unlist()

  description <-
    (x$description %||% "") %>%                     # use "" if no description is provided
    str_replace_all("\n", "") %>%                   # remove newlines
    capitalise() %>%                         # capitalise sentences
    str_replace_all("\\\\link\\[[a-zA-Z0-9_:]*\\]\\{([^\\}]*)\\}", "\\1") %>%  # substitute \link[X](Y) with just Y
    str_replace_all("[ \t\\.]*$", "")               # remove trailing whitespace and punctuation

  lis[["format"]] <- paste0(
    lis[["type"]],
    ifelse(length(lis[["default"]]) > 1, " vector", "")
  )

  extra_text <-
    lis[!names(lis) %in% c("id", "type")] %>%
    as.list() %>%
    stringr::str_glue_data("{names(.)}: {.}") %>%
    capitalise() %>%
    paste0(collapse = sep)

  paste0(description, sep, extra_text)
}

#' @export
#' @rdname parameter
as_descriptive_tibble <- function(x) {
  UseMethod("as_descriptive_tibble")
}

#' @export
as_descriptive_tibble.parameter <- function(x) {
  tibble(
    id = x$id,
    type = "abstract",
    domain = NA,
    default = collapse_set(x$default)
  )
}



capitalise <- function(string) {
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}

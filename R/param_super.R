#' Helper functions for converting parameters from and to other formats
#'
#' @param id The name of the parameter.
#' @param default The default value of the parameter.
#' @param description An optional (but recommended) description of the parameter.
#' @param ... Extra fields to be saved in the parameter.
#' @param x An object (parameter or distribution) to be converted.
#' @param li A list to be converted into a parameter.
#'
#' @seealso [character_parameter()], [integer_parameter()], [logical_parameter()], [numeric_parameter()], [range_parameter()], [subset_parameter()], [dynparam]
parameter <- function(
  id,
  default,
  ...,
  description = NULL
) {
  assert_that(is.character(id))
  assert_that(is.null(description) || is.character(description))

  param <- list(
    id = id,
    default = default,
    description = description,
    ...
  )
  class(param) <- c("parameter", "list")
  param
}

#' @export
#' @rdname parameter
as.character.parameter <- function(x, ...) {
  lis <- as_descriptive_tibble(x) %>% unlist()

  ifelse(names(lis) == "id", lis, paste0(names(lis), "=", lis)) %>% paste0(collapse = " | ")
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

#' @export
#' @rdname parameter
print.parameter <- function(x, ...) {
  cat(as.character(x))
}

#' @importFrom Hmisc capitalize
#' @importFrom stringr str_replace_all str_replace str_glue_data
get_description <- function(x) {
  lis <- as_descriptive_tibble(x) %>% unlist()

  description <-
    (x$description %||% "") %>%                     # use "" if no description is provided
    str_replace_all("\n", "") %>%                   # remove newlines
    Hmisc::capitalize() %>%            # capitalise sentences
    str_replace_all("\\\\link\\[[a-zA-Z0-9_:]*\\]\\{([^\\}]*)\\}", "\\1") %>%  # substitute \link[X](Y) with just Y
    str_replace_all("[ \t]*$", "")     # remove trailing whitespace

  if (!grepl("\\.$", description)) {
    description <- paste0(description, ".")
  }

  extra_text <-
    lis[names(lis) != "id"] %>%
    stringr::str_glue_data("{names(.)} = {.}") %>%
    paste0(collapse = "; ") %>%
    stringr::str_replace("(.*)", "(\\1)")

  paste0(description, " ", extra_text)
}

#' @export
#' @rdname parameter
as_roxygen.parameter <- function(x) {
  paste0("@param ", x$id, " ", get_description(x))
}


#' @export
#' @rdname parameter
#'
#' @importFrom optparse make_option
as_argparse.parameter <- function(x) {
  lis <- as_descriptive_tibble(x) %>% unlist()

  optparse::make_option(
    opt_str = paste0("--", x$id),
    type = "character",
    default = paste0(x$default, collapse = ","),
    help = get_description(x)
  )
}

argparse_trafo <- function(x, v) {
  UseMethod("argparse_trafo")
}

argparse_trafo.parameter <- function(x, v) {
  strsplit(v, split = ",") %>% first()
}

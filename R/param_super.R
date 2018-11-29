#' Helper functions for converting parameters from and to other formats
#'
#' @param id The name of the parameter.
#' @param default The default value of the parameter.
#' @param description An optional (but recommended) description of the parameter.
#' @param ... Extra fields to be saved in the parameter.
#' @param param A parameter to be converted.
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
  # the parameter should have its own as.character funtion defined
  class_name <- class(x)[[1]]
  class(x) <- "list"
  deparse(x) %>% gsub("^list", class_name, .)
}

#' @export
#' @rdname parameter
as_paramhelper <- function(param) {
  UseMethod("as_paramhelper")
}

#' @export
#' @rdname parameter
as_roxygen <- function(param) {
  UseMethod("as_roxygen")
}

#' @export
#' @rdname parameter
as_argparse <- function(param) {
  UseMethod("as_argparse")
}

#' @rdname parameter
#' @export
as.list.parameter <- function(x, ...) {
  assert_that(is_parameter(x))

  x$type <- gsub("_parameter", "", class(x)[[1]])
  class(x) <- "list"

  # transform distributions to list
  for (n in names(x)) {
    if ("distribution" %in% class(x[[n]])) {
      x[[n]] <- as.list(x[[n]])
    }
  }

  x
}

#' @export
#' @rdname parameter
as_parameter <- function(li) {
  # check that list has a recognised type
  assert_that("list" %in% class(li), li %has_name% "type", li$type %in% names(parameters))

  # check that all the required parameters exist
  constructor_fun <- parameters[[li$type]]
  arg_classes <- formals(constructor_fun) %>% as.list() %>% map_chr(class)
  required_args <- arg_classes %>% keep(~ . == "name") %>% names()
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

is_parameter_list <- function(li) {
  "list" %in% class(li) && li %has_name% "type" && li$type %in% names(parameters)
}

#' @export
#' @rdname parameter
print.parameter <- function(x, ...) {
  cat(as.character(x))
}

# #' @importFrom Hmisc capitalize
# to_roxygen.parameter <- function(param) {
#   description <-
#     param$description %>%
#     ifelse(!is.null(.), ., "") %>%     # use "" if no description is provided
#     str_replace_all("\n", "") %>%      # remove newlines
#     Hmisc::capitalize() %>%            # capitalise sentences
#     gsub("\\\\link\\[[a-zA-Z0-9_:]*\\]\\{([^\\}]*)\\}", "\\1", .) # substitute \link[X](Y) with just Y
#
#   range_text <-
#     case_when(
#       param$type == "discrete" ~ paste0("; values: {", paste0("`", sapply(parameter$values, deparse), "`", collapse = ", "), "}"),
#       param$type %in% c("integer", "numeric") ~ paste0("; range: from `", deparse(parameter$lower), "` to `", deparse(parameter$upper), "`"),
#       TRUE ~ ""
#     )
#
#   paste0(
#     "@param ", parameter_id, " ",
#     parameter$type, "; ", description, " (default: `",
#     deparse(parameter$default, width.cutoff = 500), "`", range_text, ")"
#   )
# }








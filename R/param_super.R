#' Abstract parameter creation function
#'
#' @param id The name of the parameter.
#' @param default The default value of the parameter.
#' @param description An optional (but recommended) description of the parameter.
#' @param ... Extra fields to be saved in the parameter.
parameter <- function(
  id,
  ...,
  description = NULL
) {
  param <- list(
    id = id,
    description = description,
    ...
  )
  class(param) <- c("parameter", "list")
  param
}

#' Helper functions for converting parameters from and to other formats
#'
#' @param param A parameter to be converted.
#' @param x An object (parameter or distribution) to be converted.
#' @param li A list to be converted into a parameter.
#'
#' @export
#'
#' @rdname parameters
#'
#' @seealso [dynparam][dynparam]
as_paramhelper <- function(param) {
  UseMethod("as_paramhelper")
}

#' @export
#' @rdname parameters
as_roxygen <- function(param) {
  UseMethod("as_roxygen")
}

#' @export
#' @rdname parameters
as_argparse <- function(param) {
  UseMethod("as_argparse")
}

#' @export
#' @rdname parameters
as_list <- function(x) {
  UseMethod("as_list")
}

#' @export
#' @rdname parameters
list_as_parameter <- function(li) {
  if (li$class == "character_parameter") {
    character_parameter(
      id = li$id,
      default = li$default,
      values = li$values,
      description = li$description,
      length = li$length
    )
  } else if (li$class == "logical_parameter") {
    logical_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      length = li$length
    )
  } else if (li$class == "integer_parameter") {
    integer_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      distribution = list_as_distribution(li$distribution),
      length = li$length
    )
  } else if (li$class == "numeric_parameter") {
    numeric_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      distribution = list_as_distribution(li$distribution),
      length = li$length
    )
  } else if (li$class == "subset_parameter") {
    subset_parameter(
      id = li$id,
      default = li$default,
      values = li$values,
      description = li$description
    )
  } else if (li$class == "range_parameter") {
    range_parameter(
      id = li$id,
      lower_default = li$lower_default,
      upper_default = li$upper_default,
      lower_distribution = list_as_distribution(li$lower_distribution),
      upper_distribution = list_as_distribution(li$upper_distribution),
      description = li$description
    )
  } else {
    stop("Unknown parameter class: ", li$class)
  }
}

#' @export
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








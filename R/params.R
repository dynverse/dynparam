# required parameter types:
# [OK] integer, character, numeric, logical
# [  ] integer_vector, character_vector, numeric_vector, logical_vector
# [  ] character_subset
# [  ] integer_range, numeric_range


parameter <- function(
  id,
  default,
  ...,
  description = NULL,
  length = 1
) {
  param <- lst(
    id,
    description,
    default,
    length,
    ...
  )
  class(param) <- c("parameter", "list")
  param
}

default_value <- function(param) {
  UseMethod("default_value", param)
}
as_paramhelper <- function(param) {
  UseMethod("as_paramhelper")
}

to_roxygen <- function(param) {
  UseMethod("to_roxygen")
}

to_argparse <- function(param) {
  UseMethod("to_argparse")
}

list_to_parameter <- function(li) {
  if (li$type %in% c("discrete", "character")) {
    character_parameter(
      id = li$id,
      default = li$default,
      values = li$values,
      description = li$description,
      length = li$length
    )
  } else if (li$type == "logical") {
    logical_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      length = li$length
    )
  } else if (li$type == "integer") {
    integer_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      distribution = list_to_distribution(li),
      length = li$length
    )
  } else if (li$type %in% c("numeric", "double")) {
    numeric_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      distribution = list_to_distribution(li),
      length = li$length
    )
  } else {
    stop("Unknown parameter type: ", li$type)
  }
}

default_value.parameter <- function(param) {
  param$default
}

as.character.parameter <- function(param) {
  distribution_str <- if (is.null(param$distribution)) "" else paste0(", . \u2208 ", as.character(param$distribution))
  default_str <- if (length(param$default) == 1) param$default else paste0("{", paste(param$default, collapse = ", "), "}")

  paste0(param$id, ": ", param$type, ", default=", default_str, distribution_str)
}

print.parameter <- function(param) {
  cat(as.character(param))
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

###########################################################
###                       NUMERIC                       ###
###########################################################
numeric_parameter <- function(
  id,
  default,
  distribution,
  description = NULL,
  length = 1
) {
  distribution$type <- "numeric"
  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description,
    length = length
  ) %>%
    extend_with("numeric_parameter", type = "numeric")
}

as_paramhelper.numeric_parameter <- function(param) {
  d2u <- distribution2uniform(param$distribution)
  u2d <- uniform2distribution(param$distribution)

  fun <- if (param$length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = d2u(param$distribution$lower),
    upper = d2u(param$distribution$upper),
    default = d2u(param$default),
    trafo = u2d
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}


###########################################################
###                       INTEGER                       ###
###########################################################
integer_parameter <- function(
  id,
  default,
  distribution,
  description = NULL,
  length = 1
) {
  distribution$type <- "integer"
  parameter(id = id, default = default, distribution = distribution, description = description, length = length) %>%
    extend_with("integer_parameter", type = "integer")
}

as_paramhelper.integer_parameter <- function(param) {
  d2u <- distribution2uniform(param$distribution)
  u2d <- uniform2distribution(param$distribution)

  fun <- if (param$length == 1) ParamHelpers::makeNumericParam else ParamHelpers::makeNumericVectorParam
  args <- list(
    id = param$id,
    lower = d2u(param$distribution$lower - .5 + 1e-10),
    upper = d2u(param$distribution$upper + .5 - 1e-10),
    default = d2u(param$default),
    trafo = function(x) round(u2d(x))
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}


###########################################################
###                      CHARACTER                      ###
###########################################################
character_parameter <- function(
  id,
  default,
  values,
  description = NULL,
  length = 1
) {
  distribution <- values
  if (is.character(distribution)) distribution <- set(values = distribution)

  parameter(id = id, default = default, distribution = distribution, description = description, length = length) %>%
    extend_with("character_parameter", type = "character")
}

as_paramhelper.character_parameter <- function(param) {
  fun <- if (param$length == 1) ParamHelpers::makeDiscreteParam else ParamHelpers::makeDiscreteVectorParam
  args <- list(
    id = param$id,
    values = param$distribution$values,
    default = param$default
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}

###########################################################
###                       LOGICAL                       ###
###########################################################
logical_parameter <- function(
  id,
  default,
  description = NULL,
  length = 1
) {
  parameter(id = id, default = default, distribution = NULL, description = description, length = length) %>%
    extend_with("logical_parameter", type = "logical")
}

as_paramhelper.logical_parameter <- function(param) {
  fun <- if (param$length == 1) ParamHelpers::makeLogicalParam else ParamHelpers::makeLogicalVectorParam
  args <- list(
    id = param$id,
    default = param$default
  )
  if (param$length != 1) args$len <- param$length
  do.call(fun, args)
}

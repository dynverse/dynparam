# required parameter types:
# [OK] integer, character, numeric, logical
# [  ] integer_vector, character_vector, numeric_vector, logical_vector
# [  ] character_subset
# [  ] integer_range, numeric_range


parameter <- function(
  id,
  default,
  ...,
  description = NULL
) {
  param <- lst(
    id,
    description,
    default,
    ...
  )
  class(param) <- c("parameter", "list")
  param
}

default_value <- function(param) {
  UseMethod("default_value", param)
}
to_paramhelper <- function(param) {
  UseMethod("to_paramhelper")
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
      description = li$description
    )
  } else if (li$type == "logical") {
    logical_parameter(
      id = li$id,
      default = li$default,
      description = li$description
    )
  } else if (li$type == "integer") {
    integer_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      distribution = list_to_distribution(li)
    )
  } else if (li$type == "numeric") {
    numeric_parameter(
      id = li$id,
      default = li$default,
      description = li$description,
      distribution = list_to_distribution(li)
    )
  } else {
    stop("Unknown parameter type: ", li$type)
  }
}

default_value.parameter <- function(param) {
  param$default
}

as.character.parameter <- function(param) {
  distribution_str <- as.character(param$distribution %||% "")
  paste0(param$id, ": ", param$type, " \u2208 ", distribution_str, ", d=", param$default)
}

print.parameter <- function(param) {
  cat(as.character(param))
}

#' @importFrom Hmisc capitalize
to_roxygen.parameter <- function(param) {
  description <-
    param$description %>%
    ifelse(!is.null(.), ., "") %>%     # use "" if no description is provided
    str_replace_all("\n", "") %>%      # remove newlines
    Hmisc::capitalize() %>%            # capitalise sentences
    gsub("\\\\link\\[[a-zA-Z0-9_:]*\\]\\{([^\\}]*)\\}", "\\1", .) # substitute \link[X](Y) with just Y

  range_text <-
    case_when(
      param$type == "discrete" ~ paste0("; values: {", paste0("`", sapply(parameter$values, deparse), "`", collapse = ", "), "}"),
      param$type %in% c("integer", "numeric") ~ paste0("; range: from `", deparse(parameter$lower), "` to `", deparse(parameter$upper), "`"),
      TRUE ~ ""
    )

  paste0(
    "@param ", parameter_id, " ",
    parameter$type, "; ", description, " (default: `",
    deparse(parameter$default, width.cutoff = 500), "`", range_text, ")"
  )
}

###########################################################
###                       NUMERIC                       ###
###########################################################
numeric_parameter <- function(
  id,
  default,
  distribution,
  description = NULL
) {
  distribution$type <- "numeric"
  parameter(
    id = id,
    default = default,
    distribution = distribution,
    description = description
  ) %>%
    extend_with(
      "numeric_parameter",
      type = "numeric"
    )
}

to_paramhelper.numeric_parameter <- function(param) {
  d2u <- distribution2uniform(param$distribution)
  u2d <- uniform2distribution(param$distribution)

  ParamHelpers::makeNumericParam(
    id = param$id,
    lower = d2u(param$lower),
    upper = d2u(param$upper),
    default = d2u(param$default),
    trafo = u2d
  )
}


###########################################################
###                       INTEGER                       ###
###########################################################
integer_parameter <- function(
  id,
  default,
  distribution,
  description = NULL
) {
  distribution$type <- "integer"
  parameter(id = id, default = default, distribution = distribution, description = description) %>%
    extend_with(
      "integer_parameter",
      type = "integer"
    )
}

to_paramhelper.integer_parameter <- function(param) {
  d2u <- distribution2uniform(param$distribution)
  u2d <- uniform2distribution(param$distribution)

  ParamHelpers::makeNumericParam(
    id = param$id,
    lower = d2u(param$lower - .5 + 1e-10),
    upper = d2u(param$upper + .5 - 1e-10),
    default = d2u(param$default),
    trafo =  function(x) round(u2d(x))
  )
}


###########################################################
###                      CHARACTER                      ###
###########################################################
character_parameter <- function(
  id,
  default,
  values,
  description = NULL
) {
  parameter(id = id, default = default, distribution = set(values = values), description = description)
}

to_paramhelper.character_parameter <- function(param) {
  ParamHelpers::makeDiscreteParam(
    id = param$id,
    values = param$values,
    default = param$default
  )
}

###########################################################
###                       LOGICAL                       ###
###########################################################
logical_parameter <- function(
  id,
  default,
  description = NULL
) {
  parameter(id = id, default = default, distribution = NULL, description = description)
}

to_paramhelper.logical_parameter <- function(param) {
  d2u <- distribution2uniform(param$distribution)
  u2d <- uniform2distribution(param$distribution)

  trafo <- if (is.null(param$trafo)) u2d else function(x) param$trafo(u2d(x))

  ParamHelpers::makeLogicalParam(
    id = param$id,
    default = param$default
  )
}

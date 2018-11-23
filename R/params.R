# required parameter types:
# integer, character, numeric, logical
# integer_vector, character_vector, numeric_vector, logical_vector
# character_subset
# integer_range, numeric_range


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
  class(param) <- c("parameter")
  param
}

default_value <- function(param) {
  UseMethod("default_value", param)
}

default_value.parameter <- function(param) {
  param$default
}

to_paramhelper <- function(param) {
  UseMethod("to_paramhelper")
}

print.parameter <- function(param) {
  cat(as.character(param))
}

to_roxygen <- function(param) {
  UseMethod("to_roxygen")
}

to_argparse <- function(param) {
  UseMethod("to_argparse")
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

as.character.numeric_parameter <- function(param) {
  paste0(param$id, ": ", param$type, " \u2208 ", as.character(param$distribution), ", d=", param$default)
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
  distribution$upper <- distribution$upper + 1 - 1e-10
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
    lower = d2u(param$lower),
    upper = d2u(param$upper),
    default = d2u(param$default),
    trafo =  function(x) floor(u2d(x))
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
  parameter(id = id, default = default, values = values, description = description)
}

to_paramhelper.character_parameter <- function(param) {
  ParamHelpers::makeDiscreteParam(
    id = param$id,
    values = param$values,
    default = param$default
  )
}

as.character.character_parameter <- function(param) {
  paste0(param$id, ": ", param$type, " \u2208 {", paste(values, collapse = ", "), ", d=", param$default)
}


###########################################################
###                       LOGICAL                       ###
###########################################################
logical_parameter <- function(
  id,
  default,
  description = NULL
) {
  parameter(id = id, default = default, distribution = distribution, trafo = trafo, description = description)
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

as.character.logical_parameter <- function(param) {
  paste0(param$id, ": ", param$type, ", d=", param$default)
}

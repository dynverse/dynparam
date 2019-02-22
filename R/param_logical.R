#' Define a logical parameter
#'
#' @inheritParams parameter
#'
#' @export
#'
#' @examples
#' logical_parameter(
#'   id = "scale",
#'   default = TRUE,
#'   description = "Whether or not to scale the input variables"
#' )
logical_parameter <- function(
  id,
  default,
  description = NULL
) {
  assert_that(is.logical(default))

  parameter(
    id = id,
    default = default,
    description = description
  ) %>%
    add_class("logical_parameter")
}

#' @export
#' @importFrom ParamHelpers makeLogicalParam makeLogicalVectorParam
as_paramhelper.logical_parameter <- function(x) {
  length <- length(x$default)

  fun <- if (length == 1) ParamHelpers::makeLogicalParam else ParamHelpers::makeLogicalVectorParam

  args <- list(
    id = x$id,
    default = x$default
  )

  if (length != 1) args$len <- length

  do.call(fun, args)
}

as_descriptive_tibble.logical_parameter <- function(x) {
  tibble(
    id = x$id,
    type = "logical",
    default = collapse_set(x$default)
  )
}

#' @export
argparse_trafo.logical_parameter <- function(x, v) {
  vec <- argparse_trafo.parameter(x, v) %>% tolower()

  case_when(
    vec %in% c("t", "true", "yes", "y", "on") ~ TRUE,
    vec %in% c("f", "false", "no", "n", "off") ~ FALSE,
    TRUE ~ NA
  )
}

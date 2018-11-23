numeric_parameter <- function(
  id,
  default,
  distribution,
  trafo = NULL
) {
  p <- lst(
    id,
    type = "numeric",
    default,
    distribution,
    trafo
  )
  class(p) <- c("numeric_parameter", "parameter")
  p
}

as.character.numeric_parameter <- function(param) {
  paste0(param$id, ": ", param$type, " \u2208 ", as.character(param$distribution), ", d=", param$default)
}

print.numeric_parameter <- function(param) {
  cat(as.character(param))
}


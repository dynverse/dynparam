check_finite <- function(x) length(x) == 1 && is.finite(x)

distribution2uniform <- function(param) {
  UseMethod("distribution2uniform", param)
}
uniform2distribution <- function(param) {
  UseMethod("uniform2distribution", param)
}

list_to_distribution <- function(li) {
  distribution <- li$distribution %||% "uniform"

  if (distribution == "uniform") {
    uniform(lower = li$lower, upper = li$upper)
  } else if (distribution == "normal") {
    normal(mean = li$mean, sd = li$sd, lower = li$lower %||% -Inf, upper = li$upper %||% Inf)
  } else if (distribution == "expuniform") {
    expuniform(lower = li$lower, upper = li$upper)
  } else {
    stop("Unknown distribution list format: ", deparse(li, width.cutoff = 100))
  }
}

###########################################################
###                       UNIFORM                       ###
###########################################################
uniform <- function(lower, upper) {
  if (!check_finite(lower)) {stop("Provide finite lower boundary when using an uniformly distributed parameter")}
  if (!check_finite(upper)) {stop("Provide finite upper boundary when using an uniformly distributed parameter")}

  p <- lst(lower, upper)
  class(p) <- c("distribution", "dist_uniform", "list")
  p
}

distribution2uniform.dist_uniform <- function(param) {
  function(q) stats::punif(q, min = param$lower, max = param$upper)
}

uniform2distribution.dist_uniform <- function(param) {
  function(p) stats::qunif(p, param$lower, param$upper)
}

as.character.dist_uniform <- function(param) {
  paste0("U(", param$lower, ", ", param$upper, ")")
}

###########################################################
###                       NORMAL                        ###
###########################################################
normal <- function(mean, sd, lower = -Inf, upper = Inf) {
  if (!check_finite(mean)) {stop("Provide finite mean when using a normal distributed parameter")}
  if (!check_finite(sd)) {stop("Provide sd when using a normal distributed parameter")}

  p <- lst(mean, sd, lower, upper)
  class(p) <- c("distribution", "dist_normal", "list")
  p
}

distribution2uniform.dist_normal <- function(param) {
  function(q) stats::pnorm(q, mean = param$mean, sd = param$sd)
}

uniform2distribution.dist_normal <- function(param) {
  function(p) stats::qnorm(p, mean = param$mean, sd = param$sd)
}

as.character.dist_normal <- function(param) {
  paste0("N(", param$mean, ", ", param$sd, ")")
}

###########################################################
###                      EXPUNIFORM                     ###
###########################################################
expuniform <- function(lower, upper) {
  if (!check_finite(lower)) {stop("Provide finite lower boundary when using an uniformly distributed parameter")}
  if (!check_finite(upper)) {stop("Provide finite upper boundary when using an uniformly distributed parameter")}

  p <- lst(lower, upper)
  class(p) <- c("distribution", "dist_expuniform", "list")
  p
}

distribution2uniform.dist_expuniform <- function(param) {
  function(q) stats::punif(log(q), min = log(param$lower), max = log(param$upper))
}

uniform2distribution.dist_expuniform <- function(param) {
  function(p) exp(stats::qunif(p, log(param$lower), log(param$upper)))
}

as.character.dist_expuniform <- function(param) {
  paste0("e^U(", sprintf("%.2f", log(param$lower)), ", ", sprintf("%.2f", log(param$upper)), ")")
}

###########################################################
###                         SET                         ###
###########################################################
set <- function(..., values = NULL) {
  p <- lst(values = c(..., values))
  class(p) <- c("distribution", "dist_set", "list")
  p
}

as.character.dist_set <- function(param) {
  paste0("{", paste(param$values, collapse = ", "), "}")
}

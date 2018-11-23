check_finite <- function(x) length(x) == 1 && is.finite(x)

distribution2uniform <- function(param) {
  UseMethod("default", param)
}
uniform2distribution <- function(param) {
  UseMethod("default", param)
}

###########################################################
###                       UNIFORM                       ###
###########################################################
uniform <- function(lower, upper) {
  if (!check_finite(lower)) {stop("Provide finite lower boundary when using an uniformly distributed parameter")}
  if (!check_finite(upper)) {stop("Provide finite upper boundary when using an uniformly distributed parameter")}

  p <- lst(lower, upper)
  class(p) <- c("distribution", "dist_uniform")
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
  class(p) <- c("distribution", "dist_normal")
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
  class(p) <- c("distribution", "dist_expuniform")
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

#' Defining, serialising and printing distributions
#'
#' Distributions are used to define the domain of an
#' [integer_parameter()] or a [numeric_parameter()].
#'
#' See the sections below for more information each of the functions.
#'
#' @section List of all currently implemented distributions:
#' * [expuniform_distribution()]
#' * [normal_distribution()]
#' * [uniform_distribution()]
#'
#' @section Serialisation:
#' * `as.list(dist)`: Converting a distribution to a list.
#' * `as_distribution(li)`: Converting a list back to a distribution.
#' * `is_distribution(x)`: Checking whether something is a distribution.
#'
#' @section Defining a distribution:
#' In order to create a new distribution named `xxx`, you need to create three functions.
#'
#' * A `xxx()` function that calls `distribution(...) %>% add_class("xxx")` at the end.
#' * `quantile_function.xxx()`: The quantile function for converting between a uniform distribution and the `xxx` distribution.
#' * `distribution_function.xxx()`: The distribution function for converting between a uniform distribution and the `xxx` distribution.
#'
#' Check the implementations of [normal_distribution()], `quantile_function.normal_distribution()`
#' and `distribution_function.normal_distribution()` for an example
#' on how to do define these functions. Alternatively, check the examples below.
#'
#' @param lower Lower limit of the distribution.
#' @param upper Upper limit of the distribution.
#' @param dist A distribution object.
#' @param li A list to be converted into a distribution.
#' @param x An object which might be a distribution.
#'
#' @param ... Fields to be saved in the distribution.
#'
#' @seealso [dynparam] for an overview of all dynparam functionality.
#'
#' @examples
#' di <- uniform_distribution(lower = 1, upper = 10)
#' print(di)
#'
#' li <- as.list(di)
#' di2 <- as_distribution(li)
#' print(di2)
#'
#' # Defining a custom distribution, using the pbeta and qbeta functions
#' beta_distribution <- function(
#'   shape1,
#'   shape2,
#'   ncp,
#'   lower = -Inf,
#'   upper = Inf
#' ) {
#'   di <- distribution(lower = lower, upper = upper, shape1, shape2, ncp)
#'   add_class(di, beta_distribution)
#' }
#'
#' distribution_function.beta_distribution <- function(dist) {
#'   function(q) {
#'     stats::pbeta(q, shape1 = dist$shape1, shape2 = dist$shape2, ncp = dist$ncp)
#'   }
#' }
#'
#' quantile_function.beta_distribution <- function(dist) {
#'   function(p) {
#'     stats::qbeta(p, shape1 = dist$shape1, shape2 = dist$shape2, ncp = dist$ncp)
#'   }
#' }
distribution <- function(
  lower,
  upper,
  ...
) {
  assert_that(
    is_single_numeric(lower), is_bounded(lower, lower_closed = TRUE),
    is_single_numeric(upper), is_bounded(upper, upper_closed = TRUE),
    lower < upper
  )

  dist <- lst(
    lower,
    upper,
    ...
  )
  class(dist) <- c("distribution", "list")
  dist
}

#' @export
#' @rdname distribution
distribution_function <- function(dist) {
  UseMethod("distribution_function", dist)
}

#' @export
#' @rdname distribution
quantile_function <- function(dist) {
  UseMethod("quantile_function", dist)
}

#' @export
as.character.distribution <- function(x, ...) {
  # the distribution should have its own as.character funtion defined
  class_name <- class(x)[[1]]
  class(x) <- "list"
  gsub("^list", class_name, deparse(x))
}

#' @export
print.distribution <- function(x, ...) {
  cat(as.character(x))
}

#' @rdname distribution
#' @export
as.list.distribution <- function(x, ...) {
  x$type <- gsub("_distribution$", "", class(x)[[1]])
  class(x) <- "list"
  x
}

#' @export
#' @rdname distribution
as_distribution <- function(li) {
  # check that list has a recognised type
  assert_that("list" %all_in% class(li), li %has_name% "type", li$type %all_in% names(distributions))

  # check that all the required parameters exist
  constructor_fun <- distributions[[li$type]]
  arg_classes <- formals(constructor_fun) %>% as.list() %>% map_chr(class)
  required_args <- arg_classes %>% keep(~ . == "name") %>% names() %>% setdiff("...")
  assert_that(li %has_names% required_args)

  # call the constructor
  do.call(constructor_fun, li[names(li) != "type"])
}

#' @export
#' @rdname distribution
is_distribution <- function(x) {
  "distribution" %in% class(x)
}
on_failure(is_distribution) <- function(call, env) {
  paste0(deparse(call$x), " is not a distribution")
}

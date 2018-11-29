#' Helper functions for converting distributions from and to other formats
#'
#' @param lower Lower limit of the distribution.
#' @param upper Upper limit of the distribution.
#' @param dist A distribution object.
#' @param li A list to be converted into a distribution.
#' @param x An object which might be a distribution.
#'
#' @param ... Fields to be saved in the distribution.
#'
#' @seealso [uniform_distribution()], [normal_distribution()], [expuniform_distribution()], [dynparam]
distribution <- function(
  lower,
  upper,
  ...
) {
  assert_that(
    is_single_numeric(lower, allow_neg_inf = TRUE, allow_pos_inf = TRUE),
    is_single_numeric(upper, allow_neg_inf = TRUE, allow_pos_inf = TRUE),
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
#' @rdname distribution
as.character.distribution <- function(x, ...) {
  # the distribution should have its own as.character funtion defined
  class_name <- class(x)[[1]]
  class(x) <- "list"
  gsub("^list", class_name, deparse(x))
}

#' @export
#' @rdname distribution
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
  assert_that("list" %in% class(li) && li %has_name% "type" && li$type %in% names(distributions))

  # check that all the required parameters exist
  constructor_fun <- distributions[[li$type]]
  arg_classes <- formals(constructor_fun) %>% as.list() %>% map_chr(class)
  required_args <- arg_classes %>% keep(~ . == "name") %>% names()
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

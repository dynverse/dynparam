#' Helper functions for converting distributions from and to other formats
#'
#' @param dist A distribution object.
#' @param li A list to be converted into a distribution.
#' @param x An object which might be a distribution.
#'
#' @param ... Fields to be saved in the distribution.
#'
#' @seealso [uniform_distribution()], [normal_distribution()], [expuniform_distribution()], [dynparam]
distribution <- function(
  ...
) {
  dist <- lst(
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
  deparse(x) %>% gsub("^list", class_name, .)
}

#' @export
#' @rdname distribution
print.distribution <- function(x, ...) {
  cat(as.character(x))
}

#' @rdname distribution
#' @export
as.list.distribution <- function(x, ...) {
  x$class <- class(x)[[1]]
  class(x) <- "list"
  x
}

#' @export
#' @rdname distribution
as_distribution <- function(li) {
  # check that list has a class
  assert_that(li %has_name% "class", is.character(li$class))

  # check that the distribution exists
  funs <- lst(
    uniform_distribution,
    expuniform_distribution,
    normal_distribution
  )
  assert_that(li$class %in% names(funs))

  # check that all the required parameters exist
  constructor_fun <- funs[[li$class]]
  arg_classes <- formals(constructor_fun) %>% as.list() %>% map_chr(class)
  required_args <- arg_classes %>% keep(~ . == "name") %>% names()
  assert_that(li %has_names% required_args)

  # call the constructor
  do.call(constructor_fun, li[names(li) != "class"])
}

#' @export
#' @rdname distribution
is_distribution <- function(x) {
  "distribution" %in% class(x)
}
on_failure(is_distribution) <- function(call, env) {
  paste0(deparse(call$x), " is not a distribution")
}

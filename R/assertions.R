###############################
###    IS SINGLE NUMERIC    ###
###############################
is_single_numeric <- function(x, allow_neg_inf = FALSE, allow_pos_inf = FALSE) {
  b <- length(x) == 1 && is.numeric(x) && (
    is.finite(x) ||
      (allow_neg_inf && x == -Inf) ||
      (allow_pos_inf && x == Inf)
  )
  b && !is.na(b)
}
on_failure(is_single_numeric) <- function(call, env) {
  left_bound <- if (eval(call$allow_neg_inf) %||% formals(is_single_numeric)$allow_pos_inf) "[" else "]"
  right_bound <- if (eval(call$allow_pos_inf) %||% formals(is_single_numeric)$allow_neg_inf) "]" else "["
  paste0(deparse(call$x), " is not a single numeric value in ", left_bound, "-Inf,Inf", right_bound)
}

###############################
###        HAS NAMES        ###
###############################
has_names <- function(x, which) {
  all(x %has_name% which)
}

on_failure(has_names) <- function(call, env) {
  paste0(deparse(call$x), " does not have names ", deparse(setdiff(eval(call$which, env), names(eval(call$x, env)))))
}

`%has_names%` <- has_names

###############################
###      IS ELEMENT OF      ###
###############################
on_failure(`%in%`) <- function(call, env) {
  table_str <- deparse(eval(call$table, env)) %>% paste0(collapse = "")
  paste0(deparse(call$x), " is not an element of ", table_str)
}

###############################
###   ALL ARE ELEMENT OF    ###
###############################
all_in <- function(x, table) {
  all(x %in% table)
}
on_failure(all_in) <- function(call, env) {
  x <- eval(call$x, env)
  table <- eval(call$table, env)
  elements <- deparse(setdiff(table, x)) %>% paste(collapse = "")
  paste0(deparse(call$x), " is missing elements ", elements)
}
`%allin%` <- all_in

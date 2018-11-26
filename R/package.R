#' Creating meta-information for parameters
#'
#' @section Parameters:
#' These functions help you provide a meta description of parameters.
#'
#' Implemented are the following functions:
#' * [character_parameter()]
#' * [integer_parameter()]
#' * [logical_parameter()]
#' * [numeric_parameter()]
#'
#' See [?parameters][dynparam::parameters] for a list of helper functions converting parameters from and to other formats.
#'
#' @section Distributions:
#' These distributions allow to define prior distributions for numeric and integer parameters.
#'
#' Implemented are the following distributions:
#' * [uniform_distribution()]
#' * [expuniform_distribution()]
#' * [normal_distribution()]
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import dynutils
#' @import purrr
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#'
#' @docType package
#' @name dynparam
NULL

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
#' * [integer_range_parameter()]
#' * [numeric_range_parameter()]
#' * [subset_parameter()]
#'
#' See [?parameter][parameter] for a list of helper functions converting parameters from and to other formats.
#'
#' @section Distributions:
#' These distributions allow to define prior distributions for numeric and integer parameters.
#'
#' Implemented are the following distributions:
#' * [uniform_distribution()]
#' * [expuniform_distribution()]
#' * [normal_distribution()]
#'
#' See [?distribution][distribution] for a list of helper functions converting parameters from and to other formats.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom tibble as_tibble as_data_frame tibble data_frame enframe deframe lst tribble
#' @import dynutils
#' @import purrr
#' @import assertthat
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#'
#' @docType package
#' @name dynparam
NULL

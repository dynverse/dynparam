#' Creating meta-information for parameters
#'
#' Provides tools for describing parameters of algorithms in an abstract way.
#' Description can include an id, a description, a domain (range or list of values),
#' and a default value. 'dynparam' can also convert parameter sets to a 'ParamHelpers' format,
#' in order to be able to use 'dynparam' in conjunction with 'mlrMBO'.
#'
#' @section Parameter set:
#' * Create a new [parameter_set()] by adding several parameters to it
#' * [as_paramhelper()]: Convert it to a ParamHelpers object
#' * [sip()]: Sample a parameter set
#'
#' @section Parameters:
#' These functions help you provide a meta description of parameters.
#'
#' Implemented are the following functions:
#' * [character_parameter()], [integer_parameter()], [logical_parameter()], [numeric_parameter()]: Creating parameters with basic R data types.
#' * [integer_range_parameter()], [numeric_range_parameter()]: Create a discrete or continuous range parameter.
#' * [subset_parameter()]: A parameter containing a subset of a set of values.
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
#' @section Advanced topics:
#' * [distribution()]: Creating a custom distribution
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

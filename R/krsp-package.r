#' krsp: Working with the Kluane Red Squirrel Project Database
#'
#' This package is designed to help users bring data from the KRSP database into
#' R. In addition, common database queries are standardized as R functions to
#' ensure consistency. Many of the functions are wrappers around \code{dplyr} or
#' \code{RMySQL} functions. The vignettes demonstrates the use of this package:
#' \code{browseVignettes(package = "krsp")}
#'
#' @name krsp
#' @docType package
#' @import dplyr
#' @importFrom assertthat assert_that
NULL

# R CMD check doesn't like %>%, this fix deals with that
globalVariables(".")

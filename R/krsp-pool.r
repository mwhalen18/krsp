#' Create krsp connection from a Pool
#'
#' The \code{pool} package is designed for managing database connectinos in
#' Shiny apps when multiple users may be creating connections to the database
#' simultaneously. This wrapper function for \code{\link[pool]{src_pool}}
#' returns a \code{krsp} database connection object from a pool.
#'
#' @param pool A \code{pool} object.
#'
#' @return A connection to the krsp database for use with \code{dplyr}.
#' @export
#' @examples
#' pool <- pool::dbPool(
#'   drv = RMySQL::MySQL(),
#'   dbname = "krsp",
#'   host = "localhost",
#'   username = "root",
#'   password = ""
#' )
#' krsp_pool(pool)
krsp_pool <- function(pool) {
  if (!requireNamespace("pool", quietly = TRUE)) {
    stop("pool package required", call. = FALSE)
  }
  con <- pool::src_pool(pool)
  class(con) <- c("krsp", class(con))
  return(con)
}



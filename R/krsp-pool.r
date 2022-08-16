#' Create krsp connection from a Pool
#'
#' The `pool` package is designed for managing database connectinos in
#' Shiny apps when multiple users may be creating connections to the database
#' simultaneously. This wrapper function for [pool::src_pool()]
#' returns a `krsp` database connection object from a pool.
#'
#' @param pool A `pool` object.
#'
#' @return A connection to the krsp database for use with `dplyr`.
#' @export
#' @examples
#' \dontrun{
#' pool <- pool::dbPool(
#'   drv = RMySQL::MySQL(),
#'   dbname = "krsp",
#'   host = "localhost",
#'   username = "root",
#'   password = ""
#' )
#' krsp_pool(pool)
#' }
krsp_pool <- function(pool) {
  if (!requireNamespace("pool", quietly = TRUE)) {
    stop("pool package required", call. = FALSE)
  }
  con <- dbplyr::src_dbi(pool::poolCheckout(pool))
  class(con) <- c("krsp", class(con))
  return(con)
}

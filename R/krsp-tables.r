#' List tables in the KRSP database
#'
#' @param con A connection to the KRSP database
#'
#' @return List of tables in KRSP database
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_tables(con)
krsp_tables <- function(con) {
  UseMethod("krsp_tables")
}

#' @export
krsp_tables.krsp <- function(con) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"))
  src_tbls(con)
}

#' Run SQL query on database
#'
#' Run a pure SQL query on the KRSP database and return a data frame. To avoid
#' accidentally changing the database, only \code{SELECT} queries are permitted.
#'
#' @param con Connection to KRSP database
#' @param sql SQL query to execute
#'
#' @return data.frame of query results
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_sql(con, "SELECT COUNT(*) AS n_squirrels FROM squirrel")
krsp_sql <- function(con, sql) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              assertthat::is.string(sql))

  # only SELECT queries are permitted
  sql <- trimws(sql)
  if (!grepl("^SELECT", sql, ignore.case = TRUE)) {
    stop("Only SELECT queries are permitted.")
  }

  DBI::dbGetQuery(con$con, sql)
}

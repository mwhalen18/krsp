#' Connect to the krsp database
#'
#' \code{krsp_connect} is a wrapper for \code{\link[dplyr]{src_mysql}} that
#' connects to the KRSP database. By default, this function will connect to a
#' local copy of the database as root user with no password. When connecting to
#' a remote database, the host, username, and password must be supplied. This
#' information can be passed as parameters to \code{krsp_connect}, however, it's
#' much safer to store these authentication parameters in a my.cnf file. Consult
#' the vignette for this package for further details.
#'
#' @param dbname database name; defaults to \code{krsp}
#' @param host,port character; host name and port of krsp database. Defaults to a local
#'    instance of the database.
#' @param user,password character; username and password. For a local instance of the
#'    database, these can typically be left as is; however, to connect to a remote
#'    instance of the krsp database these will be user specific and must be
#'    supplied.
#' @param group character; my.cnf option group
#' @param ... Additional arguments passed on to \code{\link[dplyr]{src_mysql}}.
#'
#' @return A connection to the krsp database for use with \code{dplyr}.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_tables(con)
krsp_connect <- function(dbname = "krsp", host = "localhost", port = 0L,
                         user = "root", password = "", group = NULL, ...) {
  if (is.null(group)) {
    con <- src_mysql(dbname = dbname, host = host, port = port,
                     user = user, password = password, ...)
  } else {
    con <- src_mysql(group = group, dbname = NULL, host = NULL, password = NULL,
                     user = NULL, ...)
  }
  class(con) <- c("krsp", class(con))
  con
}

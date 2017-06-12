#' Launch the Shiny Squirrels dashboard
#'
#' This Shiny app requires a connection to the KRSP database, which can be
#' accomplished in one of two ways. Either using using \code{location} to choose
#' one of the predefined connection settings or by setting the database name,
#' host, username, and password directly.
#'
#' @param location string; choose one of the predefined database connection
#'   settings. "local" will connect to a local copy of the database as root user
#'   with no password. "camp" is to be used at camp to connect to the database
#'   on the head tech computer over the local network. If \code{location} is
#'   set, then all other parameters are ignored.
#' @param dbname string; the name of the database to connect to, typically
#'   "krsp".
#' @param host string; the IP address of the computer hosting the database.
#' @param username string; username for connection.
#' @param password string; password for connection.
#' @export
#' @examples
#' \dontrun{
#' krsp_dashboard()
#' }
krsp_dashboard <- function(location, dbname = "krsp", host = "localhost",
                             username = "root", password = "") {
  assert_that(missing(location) || location %in% c("local", "camp"),
              assertthat::is.string(dbname),
              assertthat::is.string(host),
              assertthat::is.string(username),
              assertthat::is.string(password))
  # find app directory
  app_dir <- system.file("dashboard", package = "krsp")
  if (app_dir == "") {
    stop("Could not find dashboard directory. Try re-installing `krsp`.",
         call. = FALSE)
  }
  # ensure that pool is installed
  if (!requireNamespace("pool", quietly = TRUE)) {
    stop("pool is required, install with devtools::install_github('rstudio/pool')")
  }
  # database connection parameters
  if (missing(location)) {
    location = "other"
  }
  if (location == "local") {
    db_connection <- list(dbname = "krsp", host = "localhost",
                          username = "root", password = "")
  } else if (location == "camp") {
    db_connection <- list(dbname = "krsp", host = "192.168.1.104",
                          username = "squirreler", password = "")
  } else {
    db_connection <- list(dbname = dbname, host = host,
                          username = username, password = password)
  }
  assign(".dbcon", db_connection, envir = .GlobalEnv)
  shiny::runApp(app_dir, display.mode = "normal")
}

#' Top squirrelers
#'
#' Generate a list of the top squirrelers based on a variety of metrics.
#'
#' @param con Connection to KRSP database
#' @param year integer; year to search within. Defaults to all years.
#'
#' @return A data frame of squirrelers and number of trapping records, behaviour
#'   observations, and collars.
#' @export
#' @examples
#' \dontrun{
#' con <- krsp_connect()
#' krsp_top(con, year = 2014) %>%
#'   head()
#' krsp_top(con) %>%
#'   head()
#' }
krsp_top <- function(con, year) {
  # assertion on arguments
  assert_that(inherits(con, "MySQLConnection"),
              missing(year) || valid_year(year, single = TRUE))
  if (!missing(year)) {
    year_arg <- as.integer(year)
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # trapping
    trapping <- tbl(con, "trapping") %>%
      filter( ft %in% c(1, 2, 3, 7, 13),
               !is.na(obs),
               obs != "") %>%
      rename_(observer = "obs")
    if (!missing(year)) {
      trapping <- trapping %>%
        filter( year(date) == year_arg)
    }
    trapping <- trapping %>%
      count_("observer") %>%
      mutate(metric = "n_trapped") %>%
      collect()
    # behaviour
    behaviour <- tbl(con, "behaviour") %>%
      filter( mode == 1,
               !is.na(observer),
               observer != "")
    if (!missing(year)) {
      behaviour <- behaviour %>%
        filter( year(date) == year_arg)
    }
    behaviour <- behaviour %>%
      count_("observer") %>%
      collect() %>%
      mutate(metric = "n_behaviours")
    # collars
    collars <- tbl(con, "trapping") %>%
      filter( radio == 1,
               !is.na(obs),
               obs != "") %>%
      rename_(observer = "obs")
    if (!missing(year)) {
      collars <- collars %>%
        filter( year(date) == year_arg)
    }
    collars <- collars %>%
      count_("observer") %>%
      collect() %>%
      mutate(metric = "n_collars")
  })
  # combine all metrics together
  top <- bind_rows(trapping, behaviour, collars) %>%
    mutate(observer =  toupper(observer)) %>%
    tidyr::spread_("metric", "n", fill = 0) %>%
    filter( grepl("^[A-Z]{2,3}$", observer)) %>%
    select("observer", "n_trapped", "n_collars", "n_behaviours") %>%
    ungroup()
  arrange(top, desc(n_trapped),  desc(n_collars),  desc(n_behaviours))
}

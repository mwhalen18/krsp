#' Identify squirrels without breeding status
#'
#' Identify female squirrels who are missing a breeding status (`br` in the
#' `litter` table) in the given year. Note that all squirrels should
#' appear in the litter table and have a breeding status even if they did not
#' breed in a given year.
#'
#' @param con Connection to KRSP database
#' @param year integer; vector of years to search within. Defaults to current year.
#'
#' @return A tbl of squirrels, including their IDs and identifying
#'    information. Can be converted to a data.frame.
#' @export
#' @examples
#' \dontrun{
#' con <- krsp_connect()
#' krsp_needs_br(con, 2015)
#' }

#' @export
krsp_needs_br <- function(con, year = current_year()) {
  # assertions on arguments
  assert_that(inherits(con, "MySQLConnection"),
              valid_year(year))

  year <- as.integer(year)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      select("squirrel_id", "br", "yr")
    squirrel <- tbl(con, "squirrel")
  })

  # if-statment required due to dplyr bug with filter and %in%
  if (length(year) == 1) {
    litter <- filter(litter,  is.null(br),  yr == year)
  } else {
    litter <- filter(litter,  is.null(br),  yr %in% year)
  }

  inner_join(litter, squirrel, by = c("squirrel_id" = "id")) %>%
    arrange(gr, trap_date) %>%
    select("gr",
           "squirrel_id",
           "colorlft", "colorrt",
           "taglft", "tagrt",
           "locx", "locy",
           "trap_date") %>%
    collect() %>%
    mutate(squirrel_id =  as.integer(squirrel_id))
}

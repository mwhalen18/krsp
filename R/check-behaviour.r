#' Check records in behaviour table
#'
#' Perform data integrity checks on the behaviour table. Run all checks at once
#' with \code{check_bahaviour()}, or perform individual checks with the specific
#' functions outlined below.
#'
#' @param con Connection to KRSP database
#' @param grid character; one or more grids to search. Defaults to all grids.
#' @param year integer; one or more years to search within. Defaults to all
#'   years.
#' @param observer character; one or more observers to highlight errors for.
#'   Defaults to all observers.
#'
#' @section Checks:
#'
#' The following checks have been implemented:
#'
#' \itemize{
#'   \item \code{check_behaviour_loc}: all locs should conform to the standard
#'     format, e.g. A.1, 22.1, or -1.8.
#'   \item \code{check_behaviour_mode}: the \code{mode} of the observation, i.e.
#'     how it was taken, should never be empty.
#'   \item \code{check_behaviour_time}: in general, behaviour observations
#'     should all be taken between between 6am and 10pm, with the exception of
#'     nest locs, which may be night locs.
#' }
#'
#' @return A data frame of records that failed the checks.
#' @export
#' @examples
#' con <- krsp_connect()
#' # run individual checks
#' check_behaviour_time(con, grid = "AG", year = 2013)
#' check_behaviour_loc(con, grid = c("AG", "JO"), observer = "MES")
#' check_behaviour_mode(con, year = 2010:2012)
#' # or perform them all at once
#' check_behaviour(con, grid = "JO", year = 2012)
check_behaviour <- function(con, grid, year, observer) {
  UseMethod("check_behaviour")
}

#' @export
check_behaviour.krsp <- function(con, grid, year, observer) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # run individual checks
  loc_check <- check_behaviour_loc(con, grid, year, observer)
  mode_check <- check_behaviour_mode(con, grid, year, observer)
  time_check <- check_behaviour_time(con, grid, year, observer)

  # add column for check name
  loc_check <- cbind(check = rep(attr(loc_check, "check"), nrow(loc_check)),
                     loc_check, stringsAsFactors = FALSE)
  mode_check <- cbind(check = rep(attr(mode_check, "check"), nrow(mode_check)),
                      mode_check, stringsAsFactors = FALSE)
  time_check <- cbind(check = rep(attr(time_check, "check"), nrow(time_check)),
                      time_check, stringsAsFactors = FALSE)
  # combine
  as_data_frame(bind_rows(loc_check, mode_check, time_check))
}

#' @export
#' @rdname check_behaviour
check_behaviour_loc <- function(con, grid, year, observer) {
  UseMethod("check_behaviour_loc")
}

#' @export
check_behaviour_loc.krsp <- function(con, grid, year, observer) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    behaviour <- tbl(con, "behaviour") %>%
      mutate_(year = ~ year(date)) %>%
      select_("id", "grid", "year", "observer",
              "date", "time", "mode", "behaviour", "detail",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tag_left", "tag_right")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      behaviour <- filter_(behaviour, ~ grid == grid_arg)
    } else {
      behaviour <- filter_(behaviour, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      behaviour <- filter_(behaviour, ~ year == year_arg)
    } else {
      behaviour <- filter_(behaviour, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      behaviour <- filter_(behaviour, ~ observer == observer_arg)
    } else {
      behaviour <- filter_(behaviour, ~ observer %in% observer_arg)
    }
  }

  # find bad locs
  results <- collect(behaviour) %>%
    filter_(~ !valid_loc(locx),
            ~ !valid_loc(locy, alpha = FALSE)) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tag_left, tag_right, sep ="/"),
            colours = ~ ifelse(is.na(squirrel_id), NA, colours),
            tags = ~ ifelse(is.na(squirrel_id), NA, tags)) %>%
    select_("id", "grid", "year", "observer",
            "date", "time", "mode", "behaviour", "detail",
            "locx", "locy",
            "squirrel_id", "colours", "tags") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_behaviour_loc"
  if (nrow(results) == 0) {
    message("check_behaviour_loc: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_behaviour
check_behaviour_mode <- function(con, grid, year, observer) {
  UseMethod("check_behaviour_mode")
}

#' @export
check_behaviour_mode.krsp <- function(con, grid, year, observer) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    behaviour <- tbl(con, "behaviour") %>%
      mutate_(year = ~ year(date)) %>%
      select_("id", "grid", "year", "observer",
              "date", "time", "mode", "behaviour", "detail",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tag_left", "tag_right") %>%
      # find missing modes
      filter_(~ is.na(mode))
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      behaviour <- filter_(behaviour, ~ grid == grid_arg)
    } else {
      behaviour <- filter_(behaviour, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      behaviour <- filter_(behaviour, ~ year == year_arg)
    } else {
      behaviour <- filter_(behaviour, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      behaviour <- filter_(behaviour, ~ observer == observer_arg)
    } else {
      behaviour <- filter_(behaviour, ~ observer %in% observer_arg)
    }
  }

  # results
  results <- collect(behaviour) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep = "/"),
            tags = ~ paste(tag_left, tag_right, sep = "/"),
            colours = ~ ifelse(is.na(squirrel_id), NA, colours),
            tags = ~ ifelse(is.na(squirrel_id), NA, tags)) %>%
    select_("id", "grid", "year", "observer",
            "date", "time", "mode", "behaviour", "detail",
            "locx", "locy",
            "squirrel_id", "colours", "tags") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_behaviour_mode"
  if (nrow(results) == 0) {
    message("check_behaviour_mode: no errors found.")
  }
  return(results)
}
#' @export
#' @rdname check_behaviour
check_behaviour_time <- function(con, grid, year, observer) {
  UseMethod("check_behaviour_time")
}

#' @export
check_behaviour_time.krsp <- function(con, grid, year, observer) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    behaviour <- tbl(con, "behaviour") %>%
      mutate_(year = ~ year(date)) %>%
      select_("id", "grid", "year", "observer",
              "date", "time", "mode", "behaviour", "detail",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tag_left", "tag_right") %>%

      filter_(
        # exclude nest locs because could be night locs
        ~ mode != 4,
        #  behaviours before 6am and after 10pm
        ~ !(time > "06:00:00" & time < "22:00:00"))
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      behaviour <- filter_(behaviour, ~ grid == grid_arg)
    } else {
      behaviour <- filter_(behaviour, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      behaviour <- filter_(behaviour, ~ year == year_arg)
    } else {
      behaviour <- filter_(behaviour, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      behaviour <- filter_(behaviour, ~ observer == observer_arg)
    } else {
      behaviour <- filter_(behaviour, ~ observer %in% observer_arg)
    }
  }

  # results
  results <- collect(behaviour) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep = "/"),
            tags = ~ paste(tag_left, tag_right, sep = "/"),
            colours = ~ ifelse(is.na(squirrel_id), NA, colours),
            tags = ~ ifelse(is.na(squirrel_id), NA, tags)) %>%
    select_("id", "grid", "year", "observer",
            "date", "time", "mode", "behaviour", "detail",
            "locx", "locy",
            "squirrel_id", "colours", "tags") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_behaviour_time"
  if (nrow(results) == 0) {
    message("check_behaviour_time: no errors found.")
  }
  return(results)
}

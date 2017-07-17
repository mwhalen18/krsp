#' Check records in litter and juvenile tables
#'
#' Perform data integrity checks on the nest tables: litter and juvenile. Run
#' all checks at once with \code{check_nest()}, or perform individual checks
#' with the specific functions outlined below.
#'
#' @param con Connection to KRSP database
#' @param grid character; one or more grids to search. Defaults to all grids.
#' @param year integer; one or more years to search within. Defaults to all
#'   years.
#'
#' @section Checks:
#'
#' The following checks have been implemented:
#'
#' \itemize{
#'   \item \code{check_nest_loc}: the locs in the litter table should be
#'     reflos for the mother's midden, e.g. A.5, 22.0, or -1.5, and not the
#'     locations of the nests.
#'   \item \code{check_nest_n1loc}:  all locs should conform to the standard
#'     format, e.g. A.1, 22.1, or -1.8.
#'   \item \code{check_nest_n2loc}:  all locs should conform to the standard
#'     format, e.g. A.1, 22.1, or -1.8.
#'   \item \code{check_nest_dna}: DNA vial numbers should be composed of
#'     the grid, followed by the 2 digit year, then 4 numbers, e.g. KL151234.
#'     In addition, all juveniles should have 2 DNA vials, unless one of the
#'     nests was missed or the squirrel wasn't present at nest 2.
#'   \item \code{check_nest_weight}: highlight records with suspicious nest 1 or
#'     nest 2 weights. In general, nest 1s should be between 7 and 25 grams
#'     and nest 2s between 30 and 80 grams. Flagged juveniles may result from an
#'     error or a late nest.
#'   \item \code{check_nest_notch}: within a litter no two squirrels of the same
#'     sex should have the same notch combination.
#'   \item \code{check_nest_grid}: the grid associated with a litter should
#'     match the mother's grid. Mis-matches can occur if the iPod used to enter
#'     the litter is set to the wrong grid.
#' }
#'
#' @return A data frame of records that failed the checks.
#' @export
#' @examples
#' con <- krsp_connect()
#' # run individual checks
#' check_nest_loc(con, grid = "AG")
#' check_nest_n1loc(con, year = 2012)
#' check_nest_n2loc(con, year = 2012)
#' check_nest_dna(con, year = 2015)
#' check_nest_weight(con, year = 2015)
#' check_nest_notch(con, year = 2014)
#' check_nest_grid(con, year = 2006)
#' # or perform them all at once
#' check_nest(con, year = 2012) %>%
#'   count(check)
check_nest <- function(con, grid, year) {
  UseMethod("check_nest")
}

#' @export
check_nest.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # run individual checks
  loc_check <- check_nest_loc(con, grid, year)
  n1loc_check <- check_nest_n1loc(con, grid, year)
  n2loc_check <- check_nest_n2loc(con, grid, year)
  dna_check <- check_nest_dna(con, grid, year)
  weight_check <- check_nest_weight(con, grid, year)
  notch_check <- check_nest_notch(con, grid, year)
  grid_check <- check_nest_grid(con, grid, year)

  # add column for check name
  loc_check <- cbind(check = rep(attr(loc_check, "check"), nrow(loc_check)),
                     loc_check, stringsAsFactors = FALSE)
  n1loc_check <- cbind(check = rep(attr(n1loc_check, "check"),
                                   nrow(n1loc_check)),
                       n1loc_check, stringsAsFactors = FALSE)
  n2loc_check <- cbind(check = rep(attr(n2loc_check, "check"),
                                   nrow(n2loc_check)),
                       n2loc_check, stringsAsFactors = FALSE)
  dna_check <- cbind(check = rep(attr(dna_check, "check"), nrow(dna_check)),
                     dna_check, stringsAsFactors = FALSE)
  weight_check <- cbind(check = rep(attr(weight_check, "check"),
                                    nrow(weight_check)),
                        weight_check, stringsAsFactors = FALSE)
  notch_check <- cbind(check = rep(attr(notch_check, "check"),
                                   nrow(notch_check)),
                       notch_check, stringsAsFactors = FALSE)
  grid_check <- cbind(check = rep(attr(grid_check, "check"), nrow(grid_check)),
                      grid_check, stringsAsFactors = FALSE)

  # combine
  as_data_frame(bind_rows(loc_check, n1loc_check, n2loc_check, dna_check,
                          weight_check, notch_check, grid_check))
}

#' @export
#' @rdname check_nest
check_nest_loc <- function(con, grid, year) {
  UseMethod("check_nest_loc")
}

#' @export
check_nest_loc.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id),
              ~ !(is.na(date1) & is.na(tagDt))) %>%
      select_("id", "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      litter <- filter_(litter, ~ grid == grid_arg)
    } else {
      litter <- filter_(litter, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      litter <- filter_(litter, ~ year == year_arg)
    } else {
      litter <- filter_(litter, ~ year %in% year_arg)
    }
  }

  # find bad locs
  results <- collect(litter) %>%
    filter_(~ !valid_loc(locx, reflo = TRUE),
            ~ !valid_loc(locy, alpha = FALSE, reflo = TRUE)) %>%
    mutate_(date = ~ coalesce(date1, tagDt)) %>%
    arrange_("grid", "year", "date")
  attr(results, "check") <- "check_nest_loc"
  if (nrow(results) == 0) {
    message("check_nest_loc: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_nest
check_nest_n1loc <- function(con, grid, year) {
  UseMethod("check_nest_n1loc")
}

#' @export
check_nest_n1loc.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id),
              ~ !is.na(date1)) %>%
      select_("id", "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      litter <- filter_(litter, ~ grid == grid_arg)
    } else {
      litter <- filter_(litter, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      litter <- filter_(litter, ~ year == year_arg)
    } else {
      litter <- filter_(litter, ~ year %in% year_arg)
    }
  }

  # find bad locs
  results <- collect(litter) %>%
    filter_(~ !valid_loc(nx1),
            ~ !valid_loc(ny1, alpha = FALSE)) %>%
    mutate_(date = ~ coalesce(date1, tagDt)) %>%
    arrange_("grid", "year", "date")
  attr(results, "check") <- "check_nest_n1loc"
  if (nrow(results) == 0) {
    message("check_nest_n1loc: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_nest
check_nest_n2loc <- function(con, grid, year) {
  UseMethod("check_nest_n2loc")
}

#' @export
check_nest_n2loc.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id),
              ~ !is.na(tagDt)) %>%
      select_("id", "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      litter <- filter_(litter, ~ grid == grid_arg)
    } else {
      litter <- filter_(litter, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      litter <- filter_(litter, ~ year == year_arg)
    } else {
      litter <- filter_(litter, ~ year %in% year_arg)
    }
  }

  # find bad locs
  results <- collect(litter) %>%
    filter_(~ !valid_loc(nx2),
            ~ !valid_loc(ny2, alpha = FALSE)) %>%
    mutate_(date = ~ coalesce(date1, tagDt)) %>%
    arrange_("grid", "year", "date")
  attr(results, "check") <- "check_nest_n2loc"
  if (nrow(results) == 0) {
    message("check_nest_n2loc: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_nest
check_nest_weight <- function(con, grid, year) {
  UseMethod("check_nest_weight")
}

#' @export
check_nest_weight.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id),
              ~ !(is.na(date1) & is.na(tagDt))) %>%
      select_("id", "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
    juvenile <- tbl(con, "juvenile") %>%
      select_(id = "litter_id", "squirrel_id",
              "sex", "notch",
              n1_wt = "weight", n2_wt = "tagWT",
              "dna1", "dna2", "comments")
    nest <- inner_join(litter, juvenile, by = "id")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      nest <- filter_(nest, ~ grid == grid_arg)
    } else {
      nest <- filter_(nest, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      nest <- filter_(nest, ~ year == year_arg)
    } else {
      nest <- filter_(nest, ~ year %in% year_arg)
    }
  }
  # find suspicious weights
  suppressWarnings({
    results <- collect(nest) %>%
      mutate_(n2_missing = ~grepl("(missing|not found|not in|dead)",
                                  comments, ignore.case = TRUE) &
                grepl("n2|nest 2", comments, ignore.case = TRUE)) %>%
      filter_(
        # nest 1
        ~ (!is.na(date1) & (is.na(n1_wt) | n1_wt < 7 | n1_wt > 25)) |
          # nest 2
          (!is.na(tagDt) & !n2_missing &
             (is.na(n2_wt) | n2_wt < 30 | n2_wt > 80))) %>%
      select_(~ -n2_missing) %>%
      mutate_(date = ~ coalesce(date1, tagDt)) %>%
      arrange_("grid", "year", "date")
  })
  attr(results, "check") <- "check_nest_weight"
  if (nrow(results) == 0) {
    message("check_nest_weight: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_nest
check_nest_dna <- function(con, grid, year) {
  UseMethod("check_nest_dna")
}

#' @export
check_nest_dna.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id),
              ~ !(is.na(date1) & is.na(tagDt))) %>%
      select_("id", "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
    juvenile <- tbl(con, "juvenile") %>%
      select_(id = "litter_id", "squirrel_id",
              "sex", "notch",
              n1_wt = "weight", n2_wt = "tagWT",
              "dna1", "dna2", "comments")
    nest <- inner_join(litter, juvenile, by = "id")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      nest <- filter_(nest, ~ grid == grid_arg)
    } else {
      nest <- filter_(nest, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      nest <- filter_(nest, ~ year == year_arg)
    } else {
      nest <- filter_(nest, ~ year %in% year_arg)
    }
  }
  # find invalid or missing dna vial codes
  suppressWarnings({
    results <- collect(nest) %>%
      filter_(
        ~ (!is.na(date1) & (!valid_dna(dna1, grid, year) | is.na(dna1))) |
          (!is.na(tagDt) & !is.na(n2_wt) &
             (!valid_dna(dna2, grid, year) | is.na(dna2)))) %>%
      mutate_(date = ~ coalesce(date1, tagDt)) %>%
      arrange_("grid", "year", "date")
  })
  attr(results, "check") <- "check_nest_dna"
  if (nrow(results) == 0) {
    message("check_nest_dna: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_nest
check_nest_notch <- function(con, grid, year) {
  UseMethod("check_nest_notch")
}

#' @export
check_nest_notch.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # juveniles with duplicate notches
    notch_counts <- tbl(con, "juvenile") %>%
      filter_(~ !is.na(notch)) %>%
      group_by_("litter_id", "sex", "notch") %>%
      summarize_(n = ~ n()) %>%
      filter_(~ n > 1)
    juvenile <- tbl(con, "juvenile") %>%
      inner_join(notch_counts, by = c("litter_id", "sex", "notch")) %>%
      select_(id = "litter_id", "squirrel_id",
              "sex", "notch",
              n1_wt = "weight", n2_wt = "tagWT",
              "dna1", "dna2", "comments")
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id),
              ~ !(is.na(date1) & is.na(tagDt))) %>%
      select_("id", "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
    nest <- inner_join(litter, juvenile, by = "id")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      nest <- filter_(nest, ~ grid == grid_arg)
    } else {
      nest <- filter_(nest, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      nest <- filter_(nest, ~ year == year_arg)
    } else {
      nest <- filter_(nest, ~ year %in% year_arg)
    }
  }
  # collect
  suppressWarnings({
    results <- collect(nest) %>%
      mutate_(date = ~ coalesce(date1, tagDt)) %>%
      arrange_("grid", "year", "date")
  })
  attr(results, "check") <- "check_nest_notch"
  if (nrow(results) == 0) {
    message("check_nest_notch: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_nest
check_nest_grid <- function(con, grid, year) {
  UseMethod("check_nest_grid")
}

#' @export
check_nest_grid.krsp <- function(con, grid, year) {
  # assertion on arguments
  assert_that(inherits(con, "src_dbi"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    litter <- tbl(con, "litter") %>%
      filter_(~ !is.na(squirrel_id)) %>%
      select_("id", grid_litter = "grid", year = "yr",
              mother_id = "squirrel_id", litter_number = "ln", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2")
    squirrel <- tbl(con, "squirrel") %>%
      select_(mother_id = "id",
              grid_squirrel = "gr")
    nest <- inner_join(litter, squirrel, by = "mother_id") %>%
      filter_(~ grid_litter != grid_squirrel)
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      nest <- filter_(nest, ~ grid == grid_arg)
    } else {
      nest <- filter_(nest, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      nest <- filter_(nest, ~ year == year_arg)
    } else {
      nest <- filter_(nest, ~ year %in% year_arg)
    }
  }
  # find mis-matched grids
  suppressWarnings({
    results <- collect(nest) %>%
      mutate_(grid = ~ paste(grid_litter, grid_squirrel, sep = "/")) %>%
      select_("id", "grid", "year",
              "mother_id", "litter_number", "br",
              "date1",  "tagDt",
              "locx", "locy",
              "nx1", "ny1", "nx2", "ny2") %>%
      mutate_(date = ~ coalesce(date1, tagDt)) %>%
      arrange_("grid", "year", "date")
  })
  attr(results, "check") <- "check_nest_grid"
  if (nrow(results) == 0) {
    message("check_nest_grid: no errors found.")
  }
  return(results)
}

#' Check records in trapping table
#'
#' Perform data integrity checks on the trapping table. Run all checks at once
#' with \code{check_trapping()}, or perform individual checks with the specific
#' functions outlined below.
#'
#' @param con Connection to KRSP database
#' @param grid character; one or more grids to search. Defaults to all grids.
#' @param year integer; one or more years to search within. Defaults to all
#'   years.
#' @param observer character; one or more observers to highlight errors for.
#'   Defaults to all observers.
#' @param reflo logical; whether to only allow reflos (e.g. A.5, 22.0) or any
#'   valid loc (e.g. A.7, 22.1).
#' @param missing_wt logical; whether to highlights weights that are missing or
#'   only weights that are not within the typical range.
#'
#' @section Checks:
#'
#' The following checks have been implemented:
#'
#' \itemize{
#'   \item \code{check_trapping_loc}: all locs should conform to the standard
#'     format for reflos, e.g. A.5, 22.0, or -1.5. Use \code{reflo = FALSE} to
#'     allow any valid loc instead of just reflos.
#'   \item \code{check_trapping_colours}: check for valid colours.
#'   \item \code{check_trapping_tags}: check for valid tags, i.e. a letter
#'     followed by 4 numbers.
#'   \item \code{check_trapping_weight}: highlight records with suspicious bag
#'     or squirrel weights. In general, bags should be between 75 and 170 grams
#'     and squirrels between 70 and 390 grams. If \code{missing_wt = TRUE}, also
#'     highlight squirrels with missing or zero weights, unless the comments
#'     suggest they were released without handling. By default missing weights
#'     are not highlighted because there are many cases where weights are
#'     intentionally not taken.
#'   \item \code{check_trapping_collwt}: any squirrel with collar fate of 2
#'     ("rc on"), 3 ("rc off"), or 4 ("rc change") should have a collar weight
#'     of either 4 or 8 grams entered.
#'   \item \code{check_trapping_dna}: DNA vial numbers should be composed of
#'     the grid, followed by the 2 digit year, then 4 numbers, e.g. KL151234.
#'   \item \code{check_trapping_newdna}: all newly caught squirrels and
#'     squirrels with ripped tags in both ears (i.e. RIP/RIP) should have DNA.
#'   \item \code{check_trapping_fate}: all trapping records should have an
#'     associated fate. In most cases, this function identifies cases where
#'     the observer forgot to enter the "recap" fate.
#' }
#'
#' @return A data frame of records that failed the checks.
#' @export
#' @examples
#' con <- krsp_connect()
#' # run individual checks
#' check_trapping_loc(con, grid = "AG", year = 2015)
#' check_trapping_loc(con, grid = "AG", year = 2015, reflo = FALSE)
#' check_trapping_colours(con, grid = "JO", year = 2011)
#' check_trapping_tags(con, year = 2014)
#' check_trapping_weight(con, grid = "SU", year = 2015)
#' check_trapping_collwt(con, grid = "KL", year = 2015)
#' check_trapping_dna(con, grid = "KL")
#' check_trapping_newdna(con, year = 2015)
#' check_trapping_fate(con, year = 2015)
#' # or perform them all at once
#' check_trapping(con, year = 2014) %>%
#'   count(check)
check_trapping <- function(con, grid, year, observer) {
  UseMethod("check_trapping")
}

#' @export
check_trapping.krsp <- function(con, grid, year, observer) {
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
  loc_check <- check_trapping_loc(con, grid, year, observer)
  colour_check <- check_trapping_colours(con, grid, year, observer)
  tag_check <- check_trapping_tags(con, grid, year, observer)
  weight_check <- check_trapping_weight(con, grid, year, observer)
  collwt_check <- check_trapping_collwt(con, grid, year, observer)
  dna_check <- check_trapping_dna(con, grid, year, observer)
  newdna_check <- check_trapping_newdna(con, grid, year, observer)
  fate_check <- check_trapping_fate(con, grid, year, observer)

  # add column for check name
  loc_check <- cbind(check = rep(attr(loc_check, "check"), nrow(loc_check)),
                     loc_check, stringsAsFactors = FALSE)
  colour_check <- cbind(check = rep(attr(colour_check, "check"),
                                    nrow(colour_check)),
                        colour_check, stringsAsFactors = FALSE)
  tag_check <- cbind(check = rep(attr(tag_check, "check"), nrow(tag_check)),
                     tag_check, stringsAsFactors = FALSE)
  weight_check <- cbind(check = rep(attr(weight_check, "check"),
                                    nrow(weight_check)),
                        weight_check, stringsAsFactors = FALSE)
  collwt_check <- cbind(check = rep(attr(collwt_check, "check"),
                                    nrow(collwt_check)),
                        collwt_check, stringsAsFactors = FALSE)
  dna_check <- cbind(check = rep(attr(dna_check, "check"), nrow(dna_check)),
                     dna_check, stringsAsFactors = FALSE)
  newdna_check <- cbind(check = rep(attr(newdna_check, "check"),
                                    nrow(newdna_check)),
                        newdna_check, stringsAsFactors = FALSE)
  fate_check <- cbind(check = rep(attr(fate_check, "check"), nrow(fate_check)),
                      fate_check, stringsAsFactors = FALSE)

  # combine
  as_data_frame(bind_rows(loc_check, colour_check, tag_check, weight_check,
                          collwt_check, dna_check, newdna_check, fate_check))
}

#' @export
#' @rdname check_trapping
check_trapping_loc <- function(con, grid, year, observer, reflo = TRUE) {
  UseMethod("check_trapping_loc")
}

#' @export
check_trapping_loc.krsp <- function(con, grid, year, observer, reflo = TRUE) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3),
              assertthat::is.flag(reflo))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(squirrel_id)) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # find bad locs
  results <- collect(trapping) %>%
    filter_(~ !valid_loc(locx, reflo = reflo),
            ~ !valid_loc(locy, alpha = FALSE, reflo = reflo)) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_loc"
  if (nrow(results) == 0) {
    message("check_trapping_loc: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_colours <- function(con, grid, year, observer) {
  UseMethod("check_trapping_colours")
}

#' @export
check_trapping_colours.krsp <- function(con, grid, year, observer) {
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
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(squirrel_id),
              # ft 4 = trapping death
              # ft 10 = natural death
              # ft 12 = handling death
              ~ !(ft %in% c(4, 10, 12))) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # find bad colours
  results <- collect(trapping) %>%
    filter_(
      # colours shouldn't be blank
      ~ is.na(color_left) | is.na(color_right) |
        # valid colours
        !valid_color(color_left) | !valid_color(color_right)) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_colours"
  if (nrow(results) == 0) {
    message("check_trapping_colours: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_tags <- function(con, grid, year, observer) {
  UseMethod("check_trapping_tags")
}

#' @export
check_trapping_tags.krsp <- function(con, grid, year, observer) {
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
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(squirrel_id),
              # ft 4 = trapping death
              # ft 10 = natural death
              # ft 12 = handling death
              ~ !(ft %in% c(4, 10, 12))) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # find bad tags
  results <- collect(trapping) %>%
    filter_(
      # tags shouldn't be blank
      ~ is.na(tagLft) | is.na(tagRt) |
        # tags are 5 characters long and composed of #'s and DFHJM
        !valid_tag(tagLft) | !valid_tag(tagRt)) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_tags"
  if (nrow(results) == 0) {
    message("check_trapping_tags: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_weight <- function(con, grid, year, observer,
                                  missing_wt = FALSE) {
  UseMethod("check_trapping_weight")
}

#' @export
check_trapping_weight.krsp <- function(con, grid, year, observer,
                                       missing_wt = FALSE) {
  # assertion on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(grid) || valid_grid(grid),
              missing(year) || valid_year(year),
              missing(observer) || is.character(observer),
              missing(observer) || all(nchar(observer) <= 3),
              assertthat::is.flag(missing_wt))

  # must have at least one filtering criterion
  if (missing(grid) && missing(year) && missing(observer)) {
    message("No filtering criteria supplied, defaulting to current year.")
    year <- current_year()
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      # find missing collar weight
      filter_(~ !is.na(squirrel_id)) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # find bad weights
  results <- collect(trapping) %>%
    filter_(
      # missing or zero weight, unless released
      ~ (missing_wt & coalesce(wgt, 0L) == 0L &
           !grepl("^rel", comments, ignore.case = TRUE)) |
        (missing_wt & coalesce(bagWt, 0L) == 0L &
           !grepl("^rel", comments, ignore.case = TRUE)) |
        # outside of normal range
        !(coalesce(wgt, 0L) == 0L | (wgt >= 70 & wgt <= 390)) |
        !(coalesce(bagWt, 0L) == 0L | (bagWt >= 75 & bagWt <= 170))) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_weight"
  if (nrow(results) == 0) {
    message("check_trapping_weight: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_collwt <- function(con, grid, year, observer) {
  UseMethod("check_trapping_collwt")
}

#' @export
check_trapping_collwt.krsp <- function(con, grid, year, observer) {
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
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      # find missing collar weight
      filter_(~ !is.na(squirrel_id),
              ~ radio %in% c(2, 3, 4),
              ~ !(coalesce(collWt, 0) %in% c(4,8))) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # collect results
  results <- collect(trapping) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_collwt"
  if (nrow(results) == 0) {
    message("check_trapping_collwt: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_dna <- function(con, grid, year, observer) {
  UseMethod("check_trapping_dna")
}

#' @export
check_trapping_dna.krsp <- function(con, grid, year, observer) {
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
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(dna1) | !is.na(dna2)) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # find bad dna vial codes
  results <- collect(trapping) %>%
    filter_(~ !valid_dna(dna1, grid, year) | !valid_dna(dna2, grid, year)) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_dna"
  if (nrow(results) == 0) {
    message("check_trapping_dna: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_newdna <- function(con, grid, year, observer) {
  UseMethod("check_trapping_newdna")
}

#' @export
check_trapping_newdna.krsp <- function(con, grid, year, observer) {
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
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(squirrel_id),
              # new adult, new juvenile, or RIP/RIP
              ~ ft %in% c(2, 7, 13),
              ~ is.na(dna1) | is.na(dna2)) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # collect
  results <- collect(trapping) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_newdna"
  if (nrow(results) == 0) {
    message("check_trapping_newdna: no errors found.")
  }
  return(results)
}

#' @export
#' @rdname check_trapping
check_trapping_fate <- function(con, grid, year, observer) {
  UseMethod("check_trapping_fate")
}

#' @export
check_trapping_fate.krsp <- function(con, grid, year, observer) {
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
    trapping <- tbl(con, "trapping") %>%
      mutate_(year = ~ year(date)) %>%
      filter_(~ !is.na(squirrel_id),
              ~ is.na(ft)) %>%
      select_("id", grid = "gr", "year", observer = "obs",
              "date",
              "ft", "rep_con",
              "locx", "locy",
              "squirrel_id",
              "color_left", "color_right",
              "tagLft", "tagRt",
              "radio", "collar", "collWt",
              "bagWt", "wgt",
              "dna1", "dna2",
              "comments")
  })
  # filtering
  if (!missing(grid)) {
    grid_arg <- grid
    # if-statment required due to dplyr bug with filter and %in%
    if (length(grid) == 1) {
      trapping <- filter_(trapping, ~ grid == grid_arg)
    } else {
      trapping <- filter_(trapping, ~ grid %in% grid_arg)
    }
  }
  if (!missing(year)) {
    year_arg <- as.integer(year)
    # if-statment required due to dplyr bug with filter and %in%
    if (length(year) == 1) {
      trapping <- filter_(trapping, ~ year == year_arg)
    } else {
      trapping <- filter_(trapping, ~ year %in% year_arg)
    }
  }
  if (!missing(observer)) {
    observer_arg <- observer
    # if-statment required due to dplyr bug with filter and %in%
    if (length(observer) == 1) {
      trapping <- filter_(trapping, ~ observer == observer_arg)
    } else {
      trapping <- filter_(trapping, ~ observer %in% observer_arg)
    }
  }

  # collect
  results <- collect(trapping) %>%
    mutate_(colours = ~ paste(color_left, color_right, sep ="/"),
            tags = ~ paste(tagLft, tagRt, sep ="/")) %>%
    select_("id", "grid", "year", "observer",
            "date",
            "ft", "rep_con",
            "locx", "locy",
            "squirrel_id", "colours", "tags",
            "radio", "collar", "collWt",
            "bagWt", "wgt",
            "dna1", "dna2",
            "comments") %>%
    arrange_("grid", "year", "observer", "date")
  attr(results, "check") <- "check_trapping_fate"
  if (nrow(results) == 0) {
    message("check_trapping_fate: no errors found.")
  }
  return(results)
}

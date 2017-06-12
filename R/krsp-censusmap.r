#' Display a map of middens comparing this and previous census
#'
#' This function is to meant to be used to monitor progress towards completion
#' of the census. It shows all middens from the previous census and whether or
#' not they have been entered in this census.
#'
#' The user must identify which census they are completing. An August census
#' will be compared to the May census from that year, while a May census will be
#' compared to the previous year's August census.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer; year of census you're working on. Must be greater than
#'   2012 when the new census table was implemented.
#' @param census character; are you completing the may or august census?
#' @param data logical; whether to just return the data instead of a plot.
#'
#' @return An interactive plot comparing censuses, or a data frame if
#'   \code{data == TRUE}.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_censusmap(con, "JO", 2016, "may", data = TRUE) %>%
#'   head()
#' krsp_censusmap(con, "KL", 2016, "august")
krsp_censusmap <- function(con, grid, year, census, data) {
  UseMethod("krsp_censusmap")
}

#' @export
krsp_censusmap.krsp <- function(con, grid, year, census = c("august", "may"),
                                data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              valid_year(year, single = TRUE), year > 2015,
              valid_grid(grid, single = TRUE),
              assertthat::is.flag(data))
  census <- match.arg(census)

  year <- as.integer(year)
  grid_choice <- grid
  reverse_grid <- (grid_choice == "AG")
  valid_fates_old <- c(1, 2, 3, 4, 6, 9)
  # exclude floater fates and juvenile share fates
  valid_fates <- c(1, 2, 8, 15, 16, 18, 19)

  # census dates
  if (census == "may") {
    this_census <- paste0(year, "-05-15")
    last_census <- find_aug_census(con, grid_choice, year - 1L)
  } else if (census == "august") {
    this_census <- find_aug_census(con, grid_choice, year)
    last_census <- paste0(year, "-05-15")
  } else {
    stop("Invalid census, must be may or august")
  }

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    census <- tbl(con, "census") %>%
      filter_(~ gr == grid_choice,
              ~ !is.na(reflo),
              ~ sq_fate %in% valid_fates,
              ~ census_date == this_census | census_date == last_census) %>%
      select_("reflo", grid = "gr", "census_date", "locx", "locy",
              "squirrel_id", fate = "sq_fate") %>%
      collect()
  })

  # process census
  census <- census %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id),
            locx = ~ loc_to_numeric(locx),
            locy = ~ suppressWarnings(round(as.numeric(locy), 1)))
  # split into censuses
  census_last <- census %>%
    filter_(~ census_date == last_census) %>%
    select_(~ -census_date)
  census_this <- census %>%
    filter_(~ census_date == this_census) %>%
    select_("reflo", locx_new = "locx", locy_new = "locy",
            squirrel_id_new = "squirrel_id", fate_new = "fate")
  # bring current fate into last census
  results <- left_join(census_last, census_this, by = "reflo") %>%
    mutate_(locx = ~ if_else(is.na(locx) & is.na(locy), locx_new, locx),
            locy = ~ if_else(is.na(locx) & is.na(locy), locy_new, locy)) %>%
    filter_(~ !is.na(locx), ~ !is.na(locy)) %>%
    select_("reflo", "grid", "locx", "locy",
            "squirrel_id", "squirrel_id_new",
            "fate", "fate_new") %>%
    arrange_("locx", "locy")

  # either return data frame or interactive map of census
  if (data) {
    return(results)
  } else {
    return(plot_census(results, reverse_grid))
  }
}

plot_census <- function(census, reverse_grid = FALSE) {
  # no results
  if (nrow(census) == 0) {
    return("No census records found.")
  }
  # create fate factor variable for colouring
  valid_fates <- c(1, 2, 8, 15, 16, 18, 19)
  fates <- c(valid_fates, 0) %>%
    as.integer()
  fate_pal <- c("#ff7f0e", "#1f77b4", "#2ca02c", "#9467bd", "#d62728",
                "#e377c2", "#bcbd22", NA)
  fates_lbl <- c(as.character(valid_fates), "tbd")
  census <- census %>%
    mutate_(fate = ~ as.integer(fate),
            fate_factor = ~ factor(coalesce(fate_new, 0L),
                                   levels = fates,
                                   labels = fates_lbl),
            id = ~ row_number(),
            censused = ~ if_else(!is.na(fate_new), "Yes", "No"),
            censused = ~ factor(censused, levels = c("Yes", "No"))) %>%
    rename_(x = "locx", y = "locy")
  # create interactive plot
  popup <- function(x) {
    row <- census[census$id == x$id, ]
    paste(
      sprintf("<strong>Reflo:</strong> %s", row$reflo),
      sprintf("<strong>Previous Owner:</strong> %s", row$squirrel_id),
      sprintf("<strong>Current Owner:</strong> %s",
              coalesce(as.character(row$squirrel_id_new), "tbd")),
      sprintf("<strong>Previous Fate:</strong> %s", row$fate),
      sprintf("<strong>Current Fate:</strong> %s",
              coalesce(as.character(row$fate_new), "tbd")),
      sep = "<br />")
  }
  fnt <- c("Helvetica Neue", "sans-serif")
  x_ticks <- floor(min(census$x)):ceiling(max(census$x))
  y_ticks <- floor(min(census$y)):ceiling(max(census$y))
  # letter labels for x-axis
  x_labels <- data_frame(x = x_ticks + ifelse(reverse_grid, 0.2, -0.2),
                         y = ceiling(max(census$y)),
                         label = sapply(x_ticks, function(i) {
                           ifelse(i > 0 & i <= 26, LETTERS[i], i)
                         })
  )
  g <- ggvis::ggvis(census, ~x, ~y) %>%
    ggvis::layer_points(fill = ~fate_factor, shape = ~censused,
                        stroke = ~censused, strokeWidth := 3,
                        size := 100, key := ~id, opacity := 0.7) %>%
    # censused = colored, not = empty
    ggvis::scale_nominal("fill", domain = fates_lbl, range = fate_pal) %>%
    # censused = circle, not = square
    ggvis::scale_nominal("shape", domain = c("Yes", "No"),
                         range = c("circle", "square")) %>%
    # give outline to un-censused
    ggvis::scale_nominal("stroke", domain = c("No", "Yes"),
                         range = c("black", NA)) %>%
    # labels for x loc letters
    ggvis::layer_text(~x, ~y, text := ~label,
                      fontSize := 14, fontWeight := "bold",
                      data = x_labels) %>%
    # main x-axis
    ggvis::add_axis("x", title = "LocX", values = x_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    # dummy x-axis for title
    ggvis::add_axis("x", orient = "top", ticks = 0,
                    title = "", #sprintf("Rattles on %s in %i", grid, year),
                    properties = ggvis::axis_props(
                      labels = list(fontSize = 0),
                      title = list(fontSize = 24, font = fnt))) %>%
    # reverse x direction for agnes
    ggvis::scale_numeric("x", reverse = reverse_grid) %>%
    # left y-axis
    ggvis::add_axis("y", title = "LocY", values = y_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    # right y-axis
    ggvis::add_axis("y", orient = "right", title = "LocY", values = y_ticks,
                    properties = ggvis::axis_props(
                      title = list(fontSize = 16, font = fnt),
                      labels = list(fontSize = 14, font = fnt)
                    )) %>%
    # fate legend
    # ggvis::add_legend("fill", title = "Fate",
    #                   properties = ggvis::legend_props(
    #                     title = list(fontSize = 20, font = fnt),
    #                     labels = list(fontSize = 16, font = fnt)
    #                   )) %>%
    ggvis::hide_legend("fill") %>%
    ggvis::hide_legend("shape") %>%
    ggvis::hide_legend("stroke") %>%
    # popup tooltips with additional information
    ggvis::add_tooltip(popup) %>%
    ggvis::set_options(height = 650, width = 900, duration = 0)
  return(g)
}

find_aug_census <- function(con, grid, year) {
  dates <- paste0(year, c("-08-15", "-09-15"))
  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    census <- tbl(con, "census") %>%
      filter_(~ gr == grid,
              ~ census_date %in% dates) %>%
      group_by_("census_date") %>%
      summarise(n = n()) %>%
      collect()
  })
  d <- census %>%
    top_n(1, n) %>%
    {.$census_date}
  if (length(d) == 1) {
    d
  } else {
    paste0(year, "-08-15")
  }
}

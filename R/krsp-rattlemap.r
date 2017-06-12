#' Map behaviour records on grid
#'
#' Create an interative plot of rattle or feeding behaviour on given grid and in
#' given year. In the plot, squirrels are colour coded, sex is distinguished by
#' different symbols, and further information is available in a tool tip on
#' mouseover. Plotting is done using \code{ggvis}, which creates an HTML object
#' that pops up in the Viewer pane in RStudio.
#'
#' Records can be further filtered to a date range using the \code{date_range}
#' arguments, which is a vector of the from and to dates, respectively, both of
#' which must be in the same year. If \code{date_range} is provided is ignored.
#'
#' @param con Connection to KRSP database
#' @param grid character; a single grid to map
#' @param year integer
#' @param date_range vector of starting and ending dates as Date objects or
#'   character representation of date in YMD format (e.g. "2016-05-25"); only
#'   show records that are between these dates.
#' @param locx_range vector of length 2 giving upper and lower bounds of x locs,
#'   either as character (e.g. \code{c("A", "H.5")}) or numeric (e.g.
#'   \code{5.5, 10}).
#' @param locy_range vector of length 2 giving upper and lower bounds of y locs,
#'   either as character (e.g. \code{c("5", "10.5")}) or numeric (e.g.
#'   \code{5, 10.5}).
#' @param middens character; whether to show middens from the August ("august")
#'   census, the May census ("may"), or not display middens at all ("none").
#' @param data logical; if TRUE return data frame instead of plotting
#'
#' @return Displays and returns a \code{ggvis} plot of rattle locations, unless
#'   \code{data} is TRUE, in which case a data frame is returned and nothing is
#'   plotted.
#' @export
#' @examples
#' con <- krsp_connect()
#' krsp_rattlemap(con, "JO", 2014, data = TRUE) %>%
#'   head()
#' krsp_rattlemap(con, "KL", 2015)
#' # choose date range
#' krsp_rattlemap(con, "AG", date_range = c("2014-04-01", "2014-04-10"))
#' # choose loc range
#' krsp_rattlemap(con, "JO", 2010, locx_range = c("D", "H"),
#'   locy_range = c(5, 10))
krsp_rattlemap <- function(con, grid, year, date_range,
                           locx_range, locy_range,
                           middens, data) {
  UseMethod("krsp_rattlemap")
}

#' @export
krsp_rattlemap.krsp <- function(con, grid, year, date_range,
                                locx_range, locy_range,
                                middens = c("none", "august", "may"),
                                data = FALSE) {
  # assertions on arguments
  assert_that(inherits(con, "src_mysql"),
              missing(year) || valid_year(year, single = TRUE),
              missing(date_range) || length(date_range) == 2,
              missing(locx_range) || length(locx_range) == 2,
              missing(locy_range) || length(locy_range) == 2,
              valid_grid(grid, single = TRUE))
  middens <- match.arg(middens)
  # convert dates
  if (!missing(date_range)) {
    from_date <- suppressWarnings(as.Date(lubridate::ymd(date_range[1])))
    to_date <- suppressWarnings(as.Date(lubridate::ymd(date_range[2])))
    assert_that(!is.na(from_date))
    assert_that(!is.na(to_date))
    # set year for filtering
    year <- lubridate::year(from_date)
    # from and to dates must be of same year
    assert_that(lubridate::year(to_date) == year)
  }
  assert_that(valid_year(year, single = TRUE))

  # check loc ranges if provided
  if (!missing(locx_range)) {
    assert_that(valid_loc(locx_range[1]),
                valid_loc(locx_range[2]))
    locx_range <- loc_to_numeric(locx_range)
    assert_that(locx_range[1] < locx_range[2])
  }
  if (!missing(locy_range)) {
    assert_that(valid_loc(locy_range[1]),
                valid_loc(locy_range[2]))
    locx_range <- loc_to_numeric(locy_range)
    assert_that(locy_range[1] < locy_range[2])
  }

  year <- as.integer(year)
  grid_choice <- grid
  reverse_grid <- (grid_choice == "AG")

  # query for most recent trapping record
  recent_query <- sprintf(
    "SELECT
      t.squirrel_id, t.date AS trap_date,
      t.taglft, t.tagrt,
      t.color_left, t.color_right,
      s.sex
    FROM
      trapping t
      INNER JOIN squirrel s
        ON t.squirrel_id = s.id
    WHERE
      s.gr IN (%s) AND
      (t.squirrel_id, t.date) IN (
        SELECT squirrel_id, MAX(date) as max_date
        FROM trapping
        WHERE YEAR(date) = %i
        GROUP BY squirrel_id);",
    paste0("'", paste(grid, collapse = "','"), "'"), year)

  # suppressWarnings to avoid typcasting warnings
  suppressWarnings({
    # get necessary tables from database
    behaviour <- tbl(con, "behaviour") %>%
      filter_(~ grid == grid_choice,
              ~ year(date) == year,
              ~ mode %in% c(1L, 4L),
              ~ behaviour == 2L,
              ~ detail == 1L) %>%
      select_("squirrel_id", "grid", "locx", "locy", "date") %>%
      collect()
    trapping <- tbl(con, "trapping") %>%
      filter_(~ gr == grid_choice,
              ~ year(date) == year,
              ~ rattle == "R") %>%
      select_("squirrel_id", grid = "gr", "locx", "locy", "date") %>%
      collect()
    recent <- krsp_sql(con, recent_query)
    if (middens != "none") {
      if (middens == "august") {
        cdate <- paste0(year - 1, "-08-15")
      } else if (middens == "may") {
        cdate <- paste0(year, "-05-15")
      }
      census <- tbl(con, "census") %>%
        filter_(~ gr == grid_choice,
                ~ census_date == cdate) %>%
        select_("squirrel_id", grid = "gr", "locx", "locy",
                date = "census_date") %>%
        collect()
    }
  })
  results_b <- inner_join(behaviour, recent, by = "squirrel_id") %>%
    mutate_(source = ~ "behaviour")
  results_t <- inner_join(trapping, recent, by = "squirrel_id") %>%
    mutate_(source = ~ "trapping")
  results <- bind_rows(results_t, results_b)
  if (middens != "none") {
    results_m <- inner_join(census, recent, by = "squirrel_id") %>%
      mutate_(source = ~ "census")
    results <- bind_rows(results, results_m)
  }
  # full list of rattles and corresponding squirrel info
  results <- results %>%
    mutate_(squirrel_id = ~ as.integer(squirrel_id),
            x = ~ loc_to_numeric(locx),
            y = ~ suppressWarnings(round(as.numeric(locy), 1)),
            date = ~ suppressWarnings(as.Date(lubridate::ymd(date))),
            color_left = ~ ifelse(is.na(color_left) | color_left == "",
                                  "-", color_left),
            color_right = ~ ifelse(is.na(color_right) | color_right == "",
                                   "-", color_right),
            taglft = ~ ifelse(is.na(taglft) | taglft == "", "-", taglft),
            tagrt = ~ ifelse(is.na(tagrt) | tagrt == "", "-", tagrt),
            colours = ~ paste(color_left, color_right, sep = "/"),
            tags = ~ paste(taglft, tagrt, sep = "/"),
            sex = ~ factor(coalesce(sex, "?"),
                           levels = c("F", "M", "?"))) %>%
    filter_(~ !is.na(x), ~ !is.na(y)) %>%
    mutate_(id = ~ row_number()) %>%
    select_("id", "squirrel_id", "x", "y", "grid", "sex",
            "colours", "tags", "date", "trap_date", "source")

  # date filtering
  if (!missing(date_range)) {
    results <- filter_(results, ~ date >= from_date, ~ date <= to_date)
  }
  # loc filtering
  if (!missing(locx_range)) {
    results <- filter_(results, ~ x >= locx_range[1], ~ x <= locx_range[2])
  }
  if (!missing(locy_range)) {
    results <- filter_(results, ~ y >= locy_range[1], ~ y <= locy_range[2])
  }

  # either return data frame or interactive map of rattles
  if (data) {
    return(results)
  } else {
    return(plot_rattles(results, reverse_grid))
  }
}

plot_rattles <- function(rattles, reverse_grid = FALSE) {
  # no results
  if (nrow(rattles) == 0) {
    return("No rattles found.")
  }
  # create squirrel_id factor variable for colouring
  rattles <- rattles %>%
    mutate_(sid = ~ factor(squirrel_id))
  # middens present?
  all_data <- rattles
  middens <- rattles %>%
    filter_(~ source == "census")
  rattles <- rattles %>%
    filter_(~ source != "census")
  # create interactive plot
  popup <- function(x) {
    row <- rattles[all_data$id == x$id, ]
    paste(
      sprintf("<strong>Date:</strong> %s", row$date),
      sprintf("<strong>Colours:</strong> %s", row$colours),
      sprintf("<strong>Tags:</strong> %s", row$tags),
      sprintf("<strong>Last Trapped:</strong> %s", row$trap_date),
      sep = "<br />")
  }
  fnt <- c("Helvetica Neue", "sans-serif")
  x_ticks <- floor(min(all_data$x)):ceiling(max(all_data$x))
  y_ticks <- floor(min(all_data$y)):ceiling(max(all_data$y))
  # letter labels for x-axis
  x_labels <- data_frame(x = x_ticks + ifelse(reverse_grid, 0.2, -0.2),
                         y = ceiling(max(all_data$y)),
                         label = sapply(x_ticks, function(i) {
                           ifelse(i > 0 & i <= 26, LETTERS[i], i)
                         })
  )
  g <- ggvis::ggvis(rattles, ~x, ~y) %>%
    ggvis::layer_points(fill = ~sid, shape = ~sex,
                        key := ~id, opacity := 0.7) %>%
    # assign shapes to sexes
    ggvis::scale_nominal("shape", range = c("circle", "square", "diamond")) %>%
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
    ggvis::hide_legend("fill") %>%
    # visualize sex with different symbols
    ggvis::add_legend("shape", title = "Sex",
                      properties = ggvis::legend_props(
                        title = list(fontSize = 20, font = fnt),
                        labels = list(fontSize = 16, font = fnt),
                        symbols = list(fill = "black", stroke = "black", size = 100)
                      )) %>%
    # popup tooltips with additional information
    ggvis::add_tooltip(popup) %>%
    ggvis::set_options(height = 650, width = 900)

  # add middens locations if requested
  g <- ggvis::layer_points(vis = g, data = middens, stroke = ~sid,
                           fill := NA, shape = ~sex) %>%
    ggvis::hide_legend("stroke")
  return(g)
}

is_integer <- function(x) {
  all(x == as.integer(x))
}

# replace NAs with last non-NA value
replace_na <- function(x) {
  # http://stackoverflow.com/a/13810615/3591386
  ind = which(!is.na(x))
  if (is.na(x[1])) {
    ind = c(1,ind)
  }
  rep(x[ind], times = diff(c(ind, length(x) + 1) ))
}

current_year <- function() {
  as.integer(format(Sys.Date(), "%Y"))
}

as_reflo <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)
  # ensure locs are valid
  x <- ifelse(valid_loc(x), x, NA)
  y <- ifelse(valid_loc(y, alpha = FALSE), y, NA)
  # split on .
  x_split <- stringr::str_split_fixed(x, "\\.", n = 2)
  y_split <- stringr::str_split_fixed(y, "\\.", n = 2)
  # .5-9 = ".", .0-4 = ""
  x_right <- ifelse(as.numeric(x_split[, 2]) >= 5, ".", "")
  y_right <- ifelse(as.numeric(y_split[, 2]) >= 5, ".", "")
  # account for missing .x
  x_right <- coalesce(x_right, "")
  y_right <- coalesce(y_right, "")
  # combine back together
  x_reflo <- paste0(x_split[, 1], x_right)
  y_reflo <- paste0(y_split[, 1], y_right)
  reflo <- paste0(x_reflo, y_reflo)
  ifelse(is.na(x) | is.na(y), NA, reflo)
}

# functions for listing levels of factors in database
year_list <- function(con) {
  years <- NULL
  if (!missing(con)) {
    sql <- "
    SELECT DISTINCT YEAR(date) AS year
    FROM trapping
    WHERE date IS NOT NULL;
    "
    years <- tryCatch(krsp_sql(con, sql), error = function(e) NULL)
    years <- as.integer(years$year)
  }
  if (is.null(years)) {
    years <- 1984:current_year()
  }
  sort(years)
}

grid_list <- function(con) {
  grids <- NULL
  if (!missing(con)) {
    sql <- "
    SELECT gr, COUNT(*) AS n
    FROM trapping
    WHERE gr IS NOT NULL
    GROUP BY gr
    HAVING n > 5;
    "
    grids <- tryCatch(krsp_sql(con, sql), error = function(e) NULL)
    grids <- grids$gr
  }
  if (is.null(grids)) {
    grids <- c("AG", "BT", "CH", "EN", "FL", "JO", "KL",
               "LL", "LR", "RR", "SU", "SX", "UL", "UR")
  }
  sort(grids)
}

active_grids <- function(con) {
  y <- max(year_list(con))
  grids <- NULL
  if (!missing(con)) {
    sql <- "
    SELECT gr, COUNT(*) AS n
    FROM trapping
    WHERE gr IS NOT NULL AND YEAR(date) = %i
    GROUP BY gr
    HAVING n > 5;" %>%
      sprintf(y)
    grids <- tryCatch(krsp_sql(con, sql), error = function(e) NULL)
    grids <- grids$gr
  }
  if (is.null(grids)) {
    grids <- grid_list(con)
  }
  return(grids)
}

# determine the next trap date based on status and last trap date
next_trap <- function(status, trap_date) {
  status <- as.character(status)
  days_forward <- c(18, 14, 7, 3,
                    18, 3,
                    10, 10, 10)
  names(days_forward) <- c("P0", "P1", "P2", "P3",
                           "Non-breeder", "LL",
                           "Parturition", "N1", "N2")
  if (lubridate::is.Date(trap_date)) {
    to_trap <- trap_date + unname(days_forward[status])
    to_trap <- dplyr::coalesce(to_trap, trap_date)
  } else{
    to_trap <- rep(NA, length(trap_date))
  }
  return(to_trap)
}

# functions for validation

valid_year <- function(year, single = FALSE) {
  check <- all(is_integer(year) & (year >= 1984) & (year <= current_year()))
  if (single) {
    check <- check && (length(year) == 1)
  }
  return(check)
}

valid_grid <- function(grid, single = FALSE) {
  check <- all(grid %in% grid_list())
  if (single) {
    check <- check && (length(grid) == 1)
  }
  return(check)
}

valid_loc <- function(x, alpha = TRUE, reflo = FALSE) {
  if (is.numeric(x)) {
    # if a numeric vector convert to character to check
    x <- as.character(x)
  } else if (!is.character(x)) {
    # if neither numeric nor character, return FALSE
    return(rep(FALSE, length(x)))
  }
  # define regular expression
  if (alpha) {
    if (reflo) {
      regex <- "^([A-Z]|(-?[0-9]{1,2}))([.][05])?$"
    } else {
      regex <- "^([A-Z]|(-?[0-9]{1,2}))([.][0-9])?$"
    }
  } else {
    if (reflo) {
      regex <- "^-?[0-9]{1,2}([.][05])?$"
    } else {
      regex <- "^-?[0-9]{1,2}([.][0-9])?$"
    }
  }
  # perform check
  grepl(regex, x, ignore.case = TRUE)
}

valid_color <- function(x) {
  grepl("^(-|([!*BGOPRWY]|B(k|K)){1,4})$", x)
}

valid_tag <- function(x) {
  grepl("^(-|[DFHJM0-9]{1}[0-9]{4})$", x)
}

valid_dna <- function(x, grid, year) {
  year <- substr(as.character(year), 3, 5)
  # 2 letters followed by 6 numbers
  val <- grepl("^[A-Z]{2}[0-9]{6}$", x)
  # first 2 letters are grid
  val <- val & substr(x, 1, 2) == grid
  # next two are year
  val <- val & substr(x, 3, 4) == year
  # can also be NA
  val <- val | is.na(x)
  val
}

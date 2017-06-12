#' Convert loc from alphanumeric to numeric
#'
#' Convert alphanumeric grid locations to purely numeric ones, e.g. A.1 becomes 1.1.
#'
#' @param x character; vector of alphanumeric grid locations.
#'
#' @return Numeric vector of grid locations.
#' @export
#' @examples
#' loc_to_numeric(c("A.1", "T.5", "-1.1", "5.0", "AAA"))
loc_to_numeric <- function(x) {
  # assertions on arguments
  assert_that(is.character(x) || is.numeric(x))
  x <- toupper(gsub('[[:space:]]', '', x))
  x <- ifelse(valid_loc(x), x, NA)
  for (i in 1:26) {
    x <- sub(LETTERS[i], i, x, fixed = TRUE)
  }
  round(as.numeric(x), 1)
}

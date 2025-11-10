# Load necessary libraries (for documentation clarity)
library(lubridate)

#' @title Extract Year and Month from a Date
#'
#' @description This function takes a date string or object and converts it
#' into a 'year-month' (ym) format using the \code{lubridate::ym} function.
#' It assumes the date input is a string where the year and month are
#' contained within the first seven characters (e.g., "YYYY-MM-DD" or "YYYY-MM").
#'
#' @param date The input date string or date object. It should be coercible
#' to a string where the first seven characters represent "YYYY-MM".
#'
#' @return A date object representing the year and month (of class 'Date'
#' or 'POSIXct' depending on the input and \code{lubridate}'s behavior),
#' formatted as Year-Month.
#'
#' @importFrom lubridate ym
#'
#' @examples
#' # Example 1: Standard "YYYY-MM-DD" format
#' make_ym("2023-10-25")
#'
#' # Example 2: Date object (will be converted to string internally)
#' current_date <- Sys.Date()
#' make_ym(current_date)
#'
#' # Example 3: Different separator
#' make_ym("2024/05/15")
#'
make_ym <- function(date) {
  lubridate::ym(substr(date, 1, 7))
}

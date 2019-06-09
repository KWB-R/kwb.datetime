# currentDateString ------------------------------------------------------------

#' String representing the current Date
#' 
#' @param format format string containing percentage-placeholders as defined in 
#'   \code{strptime}
#' 
#' @return character string representing the current date
#' 
#' @export
#' 
#' @examples 
#' currentDateString()
#' currentDateString("%d.%m.%Y")
#' currentDateString("%Y-%m-%d")
#' 
currentDateString <- function(format = "%d %B %Y")
{
  format(Sys.Date(), format = format)
}

# currentQuarter ---------------------------------------------------------------

#' Number of current Quarter
#' 
#' @return number of current quarter (1, 2, 3 or 4)
#' @export
#' 
currentQuarter <- function()
{
  hsQua(as.integer(format(Sys.Date(), "%m")))
}

# currentYear ------------------------------------------------------------------

#' Current Year (as numeric)
#' 
#' @return Current year as four digit number (numeric)
#' 
#' @export
#' 
currentYear <- function()
{
  as.integer(format(Sys.Date(), "%Y"))
}

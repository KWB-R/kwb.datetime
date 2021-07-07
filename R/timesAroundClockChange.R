# timesAroundClockChange -------------------------------------------------------

#' Times Around Clock Change in Central Europe
#' 
#' Sequence of Times Around Clock Change in Central Europe
#' 
#' @param year year for which to demonstrate the switch between Central European
#'   Time (CET) and Central European Summer Time (CEST)
#' @param normalToSummer \code{TRUE}: CET to CEST, \code{FALSE}: CEST to CET
#' @param step_s time step in seconds
#' @param length.out number of time objects in returned vector
#' @return vector of POSIXct objects with length \code{length.out}
#' @export
#' @examples
#' timesAroundClockChange(2019, normalToSummer = TRUE)
#' timesAroundClockChange(2019, normalToSummer = FALSE)
#' timesAroundClockChange(2019, TRUE, step_s = 1, length.out = 3)
#' timesAroundClockChange(2019, FALSE, step_s = 1, length.out = 3)
timesAroundClockChange <- function(
  year = 2000, normalToSummer = TRUE, step_s = 1800, length.out = 5
)
{
  switchday <- date_range_CEST(year)[ifelse(normalToSummer, 1, 2)]
  
  as_switchday_posix <- function(x) as.POSIXct(
    paste0(switchday, "T", x), 
    format = "%Y-%m-%dT%H:%M:%S%z", 
    tz = "Europe/Berlin"
  )
  
  second_before_switch <- as_switchday_posix(
    ifelse(normalToSummer, "01:59:59+0100", "02:59:59+0200")
  )
  
  from <- second_before_switch + 1 - (length.out %/% 2) * step_s
  
  seq.POSIXt(from = from, by = step_s, length.out = length.out)
}

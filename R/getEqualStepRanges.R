# getEqualStepRanges -----------------------------------------------------------

#' Sequences of Date Time Objects With Equal Time Step
#' 
#' @param times vector of \code{POSIXct} objects
#' @export
#' @examples 
#' # Generate a sequence of date and time objects
#' times <- seq(as.POSIXct("2019-01-01"), as.POSIXct("2020-01-01"), 3600)
#' 
#' # As expected, exactly one sequence of equal time step is found:
#' getEqualStepRanges(times)
#' 
#' # Simulate the case that timestamps were read from a text file and converted
#' # with as.POSIXct()
#' timestamps <- as.character(times)
#' new_times <- as.POSIXct(timestamps)
#' 
#' # Show the sequences of equal time steps again
#' getEqualStepRanges(new_times)
#' 
#' # What happened? The timestamp 2019-10-27 02:00 appears twice! Once in CEST
#' # and once in CET. Use a helper function that assigns CEST and CET as 
#' # required:
#' good_times <- kwb.datetime::berlin_local_timestamps_to_POSIXct(timestamps)
#' 
#' # Check if the original date and time objects could be reproduced
#' identical(good_times, times)
#' 
getEqualStepRanges <- function(times) 
{
  stopifnot(inherits(times, "POSIXt"))
  
  diffs <- diff(times)
  
  diff_change_at <- which(diff(diffs) != 0)
  
  starts <- if (length(diff_change_at)) {
    c(1, diff_change_at + 1)
  } else {
    1
  }
  
  ranges <- kwb.utils::startsToRanges(starts, length(times), 0, 0)
  
  ranges$from_time = times[ranges$from]
  ranges$to_time = times[ranges$to]
  ranges$step = diffs[ranges$from]
  
  structure(kwb.utils::addClass(ranges, "equal_step_range"), times = times)
}

# getEqualStepRanges -----------------------------------------------------------

#' Sequences of Date Time Objects With Equal Time Step
#' 
#' @param times vector of \code{POSIXct} objects
#' @export
#' @examples 
#' # Generate a sequence of date and time objects
#' as_berlin_posix <- function(x) as.POSIXct(x, tz = "Europe/Berlin")
#' times <- seq(
#'   from = as_berlin_posix("2019-01-01"), 
#'   to = as_berlin_posix("2020-01-01"), 
#'   by = 3600
#' )
#' 
#' # As expected, exactly one sequence of equal time step is found:
#' getEqualStepRanges(times)
#' 
#' # Simulate the case that timestamps were read from a text file and converted
#' # with as.POSIXct()
#' timestamps <- as.character(times)
#' new_times <- as.POSIXct(timestamps, tz = "Europe/Berlin")
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
  
  structure(kwb.utils::addClass(ranges, "equalStepRange"), times = times)
}

# plot.equalStepRange ----------------------------------------------------------

#' @export
#' @keywords internal
#' 
plot.equalStepRange <- function(x, format = "%d.%m.%Y %H:%M", ...)
{
  stopifnot(inherits(x, "equalStepRange"))

  times <- kwb.utils::getAttribute(x, "times")

  xlim <- kwb.utils::hsRestoreAttributes(
    c(min(x$from_time), max(x$to_time)), 
    attribs = attributes(x$from_time)
  )

  old_par <- graphics::par(mar = c(2.5, 1, 3, 13))
  on.exit(graphics::par(old_par))

  n_periods <- nrow(x)
  
  graphics::plot(
    x$from_time[1], 1, xlim = xlim, ylim = c(n_periods, 1), type = "n",
    xlab = "", ylab = "", yaxt = "n", main = "Sequences of equal time step"
  )
  
  for (i in seq_len(n_periods)) {
    
    xx <- times[x$from[i]:x$to[i]]
    graphics::points(xx, rep(i, length(xx)))
    
    graphics::text(
      xlim[2] + 0.05 * diff(xlim), i, xpd = TRUE, cex = 0.8, adj = 0, 
      labels = sprintf(
        "%s - %s\nstep = %s", 
        format(x$from_time[i], format = format), 
        format(x$to_time[i], format = format), 
        x$step[i]
      )
    )
  }
}

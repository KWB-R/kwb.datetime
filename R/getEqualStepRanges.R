# getEqualStepRanges -----------------------------------------------------------
getEqualStepRanges <- function(times) 
{
  stopifnot(inherits(times, "POSIXt"))
  
  diffs <- diff(times)
  
  diff_change_at <- which(diff(diffs) != 0)
  
  starts <- c(if (diff_change_at[1] != 1) 1, diff_change_at + 1)
  
  ranges <- kwb.utils::startsToRanges(starts, length(times), 0, 0)
  
  ranges$from_time = times[ranges$from]
  ranges$to_time = times[ranges$to]
  ranges$step = diffs[ranges$from]
  
  structure(kwb.utils::addClass(ranges, "equal_step_range"), times = times)
}

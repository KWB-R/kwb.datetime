# date_range_CEST --------------------------------------------------------------

#' When does Summer Time start / end?
#' 
#' At what days does the summer time start /end in a given year?
#' 
#' @param year Scalar year number between 1980 and 2100. 
#' 
#' @export
#' 
#' @examples 
#' # At what days does summer time start and end, respectively, in 2010?
#' date_range_CEST(2010)
#'   
#' # Check if summer time really starts at 2010-03-28. Timestamps between
#' # 2:00 (inclusive) and 3:00 (exclusive) do not exist in Central European Time
#' # Note that in this case R removes the time information!
#' as.POSIXct("2010-03-28 01:59:59", tz = "Europe/Berlin") # CET
#' as.POSIXct("2010-03-28 02:00:00", tz = "Europe/Berlin") # Time removed!
#' as.POSIXct("2010-03-28 02:59:59", tz = "Europe/Berlin") # Time removed!
#' as.POSIXct("2010-03-28 03:00:00", tz = "Europe/Berlin") # CEST
#' 
#' # Check if summer time really ends at "2010-10-31. Timestamps between
#' # 2:00 (inclusive) and 3:00 (exclusive) exist twice, once in CEST and a 
#' # second time in CET, so R does not know which one you mean! 
#' as.POSIXct("2010-10-31 01:00:00", tz = "Europe/Berlin") # CEST
#' as.POSIXct("2010-10-31 02:00:00", tz = "Europe/Berlin") # CEST
#' 
#' # R seems to decide (on my computer!) that times before 02:01:50 belong to
#' # CEST and at or after that time belong to CET!
#' as.POSIXct("2010-10-31 02:01:49", tz = "Europe/Berlin") # CEST
#' as.POSIXct("2010-10-31 02:01:50", tz = "Europe/Berlin") # CET
#' 
#' as.POSIXct("2010-10-31 02:02:00", tz = "Europe/Berlin") # CET
#' as.POSIXct("2010-10-31 03:00:00", tz = "Europe/Berlin") # CET
#' 
#' # Get the starts and ends of CEST for a sequence of years
#' date_range_CEST(2017:2020)
#' 
date_range_CEST <- function(year)
{
  stopifnot(is.numeric(year), all(year >= 1980), all(year <= 2100))
  
  if (length(year) > 1) {
    result <- do.call(rbind, lapply(year, date_range_CEST))
    rownames(result) <- year
    return (result)
  }
  
  # Range of days in which to expect the begin / end of summer time (seems
  # to be valid at least between 1980 and 2100)
  day_range <- list(begin = c("03-25", "04-07"), end = c("09-24", "11-01"))
  
  # Sequences of days in which to expect the begin / end of summer time
  candidates <- lapply(day_range, function(x) {
    date_range  <- as.Date(paste0(year, "-", x))
    seq.Date(date_range[1], date_range[2], by = 1)
  })
  
  # First for the "begin", then for the "end" of summer time, convert the
  # candidate days to POSIXct and evaluate the first / last occurrence of 
  # UTC offset "+0200" (summer time)
  sapply(names(candidates), function(key) {
    days <- paste0(year, "-", substr(as.character(candidates[[key]]), 6, 10))
    indices <- grep("\\+0200", format(as.POSIXct(days, tz = "CET"), "%z"))
    days[ifelse(key == "begin", indices[1] - 1, indices[length(indices)])]
  })
}

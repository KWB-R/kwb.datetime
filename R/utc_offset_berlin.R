# utc_offset_Berlin_time -------------------------------------------------------

#' UTC Offsets of Berlin Local Timestamps
#' 
#' For local timestamps (character) in the format \code{"yyyy-mm-dd HH:MM:SS"},
#' of which is known that they are recorded in time zone Europe/Berlin, i.e. CET
#' in winter and CEST in summer, the UTC offset (i.e. \code{"+0100"} in winter
#' and \code{"+0200"} in summer) is determined. Therfore, it is required that
#' the \code{timestamps} are ordered by time, which should be the case if they 
#' were recorded by a measuring device. Use this function to create unique 
#' timestamps by adding their UTC offset.
#' 
#' @param timestamps vector of character representing timestamps in format
#'   \code{"yyyy-mm-dd HH:MM:SS"}
#'   
#' @return vector of elements \code{"+0100"} or \code{"+0200"}, depending on 
#'   whether the timestamps at corresponding positions in \code{timestamps} are
#'   in CET or CEST, respectively.
#'
#' @export
#' 
#' @examples 
#' # Change from CET to CEST
#' utc_offset_Berlin_time(c(
#'   "2017-03-26 01:58:00",
#'   "2017-03-26 01:59:00",
#'   "2017-03-26 03:00:00", # jump from 02:00 to 03:00
#'   "2017-03-26 03:01:00",
#'   "2017-03-26 03:02:00"
#' ))
#'   
#' #> "+0200" "+0200" "+0100" "+0100" "+0100"
#' 
#' # Note that the following timestamps do in fact not exist but do
#' # nevertheless result in "+0200"
#' utc_offset_Berlin_time(c(
#'   "2017-03-26 02:00:00",
#'   "2017-03-26 02:15:00",
#'   "2017-03-26 02:30:00",
#'   "2017-03-26 02:45:00"
#' ))
#' 
#' #> "+0200" "+0200" "+0200" "+0200"
#' 
#' # Change from CEST to CET
#' utc_offset_Berlin_time(c(
#'   "2017-10-29 01:30:00", # CEST
#'   "2017-10-29 02:00:00", # first time: CEST
#'   "2017-10-29 02:30:00", # first time: CEST
#'   "2017-10-29 02:00:00", # second time: CET
#'   "2017-10-29 02:30:00"  # second time: CET
#' ))
#'   
#'  #> "+0200" "+0200" "+0200" "+0100" "+0100"
#'
utc_offset_Berlin_time <- function(timestamps)
{
  stopifnot(all(hasTimeFormat(timestamps, "%Y-%m-%d %H:%M:%S")))
  
  # Extract the day strings
  days <- substr(timestamps, 1, 10)
  
  # What different days are concerned?
  unique_days <- unique(days)
  
  # What are the UTC offsets for these days (NA for a day with change in offset)
  unique_offsets <- utc_offset_Berlin_day(unique_days)
  
  # Initialise the output vector
  offsets <- character(length(timestamps))
  
  # For which days the offset is known?
  is_known <- ! is.na(unique_offsets)
  
  # Set the offset for timestamps belonging to the days of known offset
  for (i in which(is_known)) {
    
    offsets[days == unique_days[i]] <- unique_offsets[i]
  }
  
  # Set the offset for timestamps belonging to the days of unknown offset
  for (i in which(! is_known)) {
    #i <- 2
    
    # Indices of timestamps belonging to the current day
    indices <- which(days == unique_days[i])
    
    offsets[indices] <- utc_offset_Berlin_time_1d(x = timestamps[indices])
  }
  
  offsets
}

# utc_offset_Berlin_time_1d ----------------------------------------------------

#' UTC Offsets of Berlin Local Timestamps within one Day
#' 
#' @param x vector of character representing timestamps in format 
#'   \code{"yyyy-mm-dd HH:MM:SS"}. All timestamps must belong to one and the
#'   same day.
#'
#' @return vector of elements \code{"+0100"} or \code{"+0200"}, depending on 
#'   whether the timestamps at corresponding positions in \code{x} are
#'   in CET or CEST, respectively.
#'
#' @seealso \code{\link{utc_offset_Berlin_time}}
#' 
#' @keywords internal
#' 
#' @examples
#' kwb.datetime:::utc_offset_Berlin_time_1d(c(
#'   "2017-10-29 00:00:00",
#'   "2017-10-29 01:00:00",
#'   "2017-10-29 02:00:00",
#'   "2017-10-29 02:00:00",
#'   "2017-10-29 03:00:00"
#' ))
#' 
#' # The following results in an error because the timestamps 
#' # belong to diffeernt days
#' # kwb.datetime:::utc_offset_Berlin_time_1d(c(
#' #   "2017-10-29 00:00:00", 
#' #   "2017-10-30 00:00:00"
#' # ))
#' 
utc_offset_Berlin_time_1d <- function(x)
{
  #cest_begin_end <- kwb.datetime::date_range_CEST(2019)
  #tz <- "Europe/Berlin"
  #seq(as.POSIXct(paste0(cest_begin_end[1], " 01:00:00")),
  #    as.POSIXct(paste0(cest_begin_end[1], " 04:00:00")), 
  #    by = 900)
      
  # x <- c(
  #   "2019-03-31 01:00:00", # CET
  #   "2019-03-31 01:15:00", # CET
  #   "2019-03-31 01:30:00", # CET
  #   "2019-03-31 02:05:00", # -> error
  #   "2019-03-31 01:45:00", # CET
  #   "2019-03-31 03:00:00", # CEST
  #   "2019-03-31 03:15:00", # CEST
  #   "2019-03-31 03:30:00"  # CEST
  # )
  # 
  # seq(as.POSIXct(paste0(cest_begin_end[2], " 01:00:00"), tz = tz),
  #    as.POSIXct(paste0(cest_begin_end[2], " 04:00:00"), tz = tz),
  #    by = 1800)
  # 
  # x <- c(
  #   "2019-10-27 01:00:00", # CEST
  #   "2019-10-27 01:30:00", # CEST
  #   "2019-10-27 02:00:00", # CEST
  #   "2019-10-27 02:30:00", # CEST
  #   "2019-10-27 02:00:00", # CET
  #   "2019-10-27 02:30:00", # CET
  #   "2019-10-27 03:00:00", # CET
  #   "2019-10-27 03:30:00", # CET 
  #   "2019-10-27 04:00:00"  # CET
  # )
    
  # all timestamps are expected to belong to the same day
  stopifnot(is.character(x))
  stopifnot(all(hasTimeFormat(x, "%Y-%m-%d %H:%M:%S")))
            
  unique_daystrings <- unique(substr(x, 1, 10))
  
  stopifnot(length(unique_daystrings) == 1)

  # Do we switch from summer to winter or from winter to summer?
  utc_offset_day <- utc_offset_Berlin_day(unique_daystrings)
  
  # If the day is the day of switching from summer to winter or winter to 
  # summer (utc_offset_day is NA in that case), ask for the day before
  if (is.na(utc_offset_day)) {
    utc_offset_day <- utc_offset_Berlin_day(
      as.character(as.Date(unique_daystrings) -1)
    )
  }
  
  stopifnot(! is.na(utc_offset_day))
  
  summer_to_winter <- utc_offset_day == "+0200"
  
  # Extract the hour as a number
  hours <- as.integer(substr(x, 12, 13))
  
  # Initialise the result vector
  offsets <- character(length(x))
  
  # Set the offsets for times before 02:00 or after 02:59
  offsets[hours < 2] <- ifelse(summer_to_winter, "+0200", "+0100")
  offsets[hours > 2] <- ifelse(summer_to_winter, "+0100", "+0200")
  
  # Timestamps within 02:00 and 02:59 can occur as CEST and as CET. Their offset
  # is initially unknown
  unknown <- (hours == 2)
  
  # Return if there are no unknown timestamps
  if (! any(unknown)) {
    return(offsets)
  }
  
  # We do not expect times between 02:00 and 03:00 if we switch from winter to
  # summer time (when we jump from 01:59 directly to 03:00)
  if (! summer_to_winter) {
    stop(
      "The following timestamps do not exist in Europe/Berlin on ", 
      unique_daystrings, ":\n", 
      kwb.utils::stringList(utils::head(x[unknown]), collapse = "\n"), 
      call. = FALSE
    )
  }
  
  # Extract the minutes part of the timestamps of unknown offset as a number
  minutes <- as.integer(substr(x[unknown], 15, 16))
  
  # Find the index where the next minute is smaller than or equal to the 
  # current minute
  split_index <- which(diff(minutes) <= 0)
  
  if (length(split_index) > 0) {
    
    if (length(split_index) > 1) {
      
      stop(
        "More than one negative time difference within the same hour ",
        "'02:00-02:59'!"
      )
    }
    
    # Set the offset for the second half to "+0100" (CET)
    offsets[unknown][(split_index + 1):sum(unknown)] <- "+0100"
    
  } else {
    
    # Set the split index to the last index
    split_index <- length(unknown)
  }
  
  # Set the offset for the first half to "+0200" (CEST)
  offsets[unknown][seq_len(split_index)] <- "+0200"
  
  offsets  
}

# utc_offset_Berlin_day --------------------------------------------------------

#' UTC Offsets of Days in Time Zone Europe/Berlin
#' 
#' @param x vector of character representing date strings in the format
#'   \code{yyyy-mm-dd}
#'
#' @return vector of elements \code{"+0100"} or \code{"+0200"}, depending on 
#'   whether all timestamps of the days at corresponding positions in \code{x}
#'   are in winter (CET) or summer (CEST), respectively. For days at which the
#'   time is adjusted from CET to CEST or vice versa, \code{NA} is returned.
#'
#' @seealso \code{\link{utc_offset_Berlin_time}}
#' 
#' @keywords internal
#' 
#' @examples
#' kwb.datetime:::utc_offset_Berlin_day("2017-11-04") #> "+0100" -> CET
#' 
#' # The offset is not unique at the days of clock change CEST -> CET
#' kwb.datetime:::utc_offset_Berlin_day("2017-10-28") #> "+0200" -> CEST
#' kwb.datetime:::utc_offset_Berlin_day("2017-10-29") #> NA -> offset not unique!
#' kwb.datetime:::utc_offset_Berlin_day("2017-10-30") #> "+0100" -> CET
#' 
#' # The offset is not unique at the days of clock change CET -> CEST
#' kwb.datetime:::utc_offset_Berlin_day("2017-03-25") #> "+0100" -> CET
#' kwb.datetime:::utc_offset_Berlin_day("2017-03-26") #> NA -> offset not unique!
#' kwb.datetime:::utc_offset_Berlin_day("2017-03-27") #> "+0200" -> CEST
#' 
utc_offset_Berlin_day <- function(x)
{
  stopifnot(is.character(x), all(hasTimeFormat(x, "%Y-%m-%d")))

  get_offset <- function(x) format(as.POSIXct(x, tz = "Europe/Berlin"), "%z")

  offset_1 <- get_offset(paste(x, "00:00:00"))
  offset_2 <- get_offset(paste(x, "12:00:00"))
  
  offset_1[offset_1 != offset_2] <- NA
  
  offset_1
}

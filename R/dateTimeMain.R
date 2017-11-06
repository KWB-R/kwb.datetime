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
  day_range <- list(
    begin = c("03-25", "04-07"),
    end = c("09-24", "11-01")
  )
  
  # Sequences of days in which to expect the begin / end of summer time
  candidates <- lapply(day_range, function(x) {
    from <- as.Date(paste0(year, "-", x[1]))
    to <- as.Date(paste0(year, "-", x[2]))
    seq(from, to, by = 1)
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
#' 
#' @keywords internal
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
#' @keywords internal
#' 
currentYear <- function()
{
  as.integer(format(Sys.Date(), "%Y"))
}

# matchingTimeFormat -----------------------------------------------------------

#' Find Time Format matching a Timestamp
#' 
#' @param timestamp character timestamp to be checked against different possible
#'   timstamp formats
#' @param timeFormats vector of possible time formats with placeholders %Y
#'   (year), %m (month), %d (day), %H (hours), %M (minutes), %S (seconds) as
#'   described for \code{format.POSIXct}
#'   
#' @return first time format in \emph{timeformats} that matches the given 
#'   \emph{timestamp}. NULL is returned if none of the given \emph{timeformats} 
#'   matches.
#'
#' @export
#' 
#' @examples 
#' # Convert a character timestamp of which the format can be one of two
#' # possible formats into a POSIXct-object
#' possibleFormats <- c("%d.%m.%Y", "%d/%m/%Y")
#'
#' x <- "14.01.2015"
#' t1 <- hsToPosix(x, format = matchingTimeFormat(x, possibleFormats))
#'   
#' # In fact this is what stringToPosix does (for only one timestamp)
#' t2 <- stringToPosix(x, formats = possibleFormats)
#' stopifnot(identical(t1, t2))
#'   
#' # You get a warning if none of the possible formats matches
#' matchingTimeFormat("01.14.2015", possibleFormats)
#'
matchingTimeFormat <- function(timestamp, timeFormats)
{
  if (length(timestamp) != 1 || ! is.character(timestamp)) {
    stop("Exactly one character timestamp expected!")
  }
  
  ok <- which(sapply(timeFormats, FUN = function(x) {
    hasTimeFormat(timestamp, x)}, USE.NAMES = FALSE)
  )
  
  if (length(ok) == 0) {
    
    warning(
      sprintf("The timestamp '%s' does not comply with any of the" , timestamp),
      sprintf(
        "assumed timestamp formats (%s)", kwb.utils::stringList(timeFormats)
      )
    )
    
    NULL
    
  } else {
    
    timeFormats[ok[1]]  
  }
}

# hasTimeFormat ----------------------------------------------------------------

#' Do Timestamps have the expected Format?
#' 
#' Checks if timestamps are formatted according to timeformat
#' 
#' @param timestamps character strings representing timestamps.
#' @param timeformat character string giving a date-time format as used by 
#'   \code{\link{strptime}}.
#'
#' @export
#' 
#' @examples 
#' hasTimeFormat("1.1.2012", "%d.%m.%Y") # TRUE
#' hasTimeFormat("1.13.2012", "%d.%m.%Y") # FALSE
#' hasTimeFormat("1/31/2012", "%m/%d/%Y") # TRUE
#' hasTimeFormat("31/1/2012", "%m/%d/%Y") # FALSE
#' 
#' hasTimeFormat(c("1.1.", "1.13.", "12.12.", "32.1."), "%d.%m.") 
#' #               TRUE     FALSE    TRUE      FALSE
#' 
hasTimeFormat <- function(timestamps, timeformat = NULL)
{
  # set default time format (it seems that inlinedocs does not like the default
  # assignment in the argument definition above)
  timeformat <- kwb.utils::defaultIfNULL(timeformat, "%Y-%m-%d %H:%M:%S")
  
  pattern <- sprintf("^%s$", timeFormatToRegex(timeformat))
  
  grepl(pattern, timestamps)
}

# timeFormatToRegex ------------------------------------------------------------

#' Time Format String to Regular Expression Pattern
#' 
#' Convert a time format string to a regular expression pattern that can be used
#' to check if a date or time string complies with a certain format.
#' 
#' @param timeformat time format string as used in R's format function with
#'   placeholders such as "%y" (year), "%m" (month), "%d" (day), "%H" (hour),
#'   etc., see \code{\link{as.POSIXct}}
#'   
#' @keywords internal
#' 
timeFormatToRegex <- function(timeformat)
{
  kwb.utils::multiSubstitute(timeformat, list(
    "\\." = "[.]",
    "%d" = "(0?[1-9]|[12][0-9]|3[01])",
    "%m" = "(0?[1-9]|1[012])", 
    "%(y|H|M|S)" = "\\\\d{2}", 
    "%Y" = "(19|20)\\\\d{2}"
  ))
}

# reformatTimestamp ------------------------------------------------------------

#' Convert Timstamp String from one Format to another
#' 
#' @param x vector of timestamps (character)
#' @param old.format format in which timestamps in \code{x} are given. Default:
#'   \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param new.format format to be applied to timestamps. Default: 
#'   \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#'   
#' @export
#' 
reformatTimestamp <- function(x, old.format = NULL, new.format = NULL)
{
  stopifnot(is.character(x))
  
  # set default time formats (it seems that inlinedocs does not like the default
  # assignment in the argument definition above)
  old.format <- kwb.utils::defaultIfNULL(old.format, "%Y-%m-%d %H:%M:%S")
  new.format <- kwb.utils::defaultIfNULL(new.format, "%Y-%m-%d %H:%M:%S")

  # check if all timestamps have the expected format (old.format)
  invalid <- which(!hasTimeFormat(x, old.format))
  
  if (length(invalid) > 0) {
    
    warning(sprintf(
      "%d timestamps do not comply with the format '%s':\n%s",
      length(invalid), old.format, 
      kwb.utils::stringList(utils::head(x[invalid]), collapse = "\n")
    ))
  }  
  
  timestamps <- as.POSIXct(x, format = old.format, tz = "UTC")
  
  invalid <- which(is.na(timestamps))
  
  if (length(invalid)) {
    
    warning(
      sprintf(
        "%d timestamps are NA after conversion to POSIXct with format '%s'.\n",
        length(invalid), old.format
      ),
      "The original timestamps look like this:\n",
      kwb.utils::stringList(utils::head(x[invalid]), collapse = "\n")
    )
  }
  
  format(timestamps, format = new.format)
}

# daylightSavingTimeInEffect ---------------------------------------------------

#' Is Daylight Saving Time (DST) in Effect?
#' 
#' @param x object of class POSIXt (either POSIXct or POSIXlt)
#' @param tz time zone string given to \code{\link{as.POSIXlt}}
#'  
#' @return Returns (hopefully!) \code{TRUE} if daylight saving time is in effect
#'   at the given time, \code{FALSE} if daylight saving time is not in effect
#'   and \code{NA} if it is unknown
#'
#' @keywords internal
#' 
daylightSavingTimeInEffect <- function(x, tz = "")
{
  stopifnot("POSIXt" %in% class(x))
  
  dst <- as.POSIXlt(x, tz = tz)$isdst
  
  ifelse(dst > 0, TRUE, ifelse(dst == 0, FALSE, NA))
}

# hsTsInfo ---------------------------------------------------------------------

#' Full Information on Timestamps
#' 
#' different representations of timestamp(s), including POSIXct-objects for 
#' local and UTC timezone as well as ISO-8601 compliant (text) timestamp and 
#' information if daylight savings time is in effect
#' 
#' @param tstamp (vector of) timestamp(s), either of POSIXt-type or character
#' @param tzone time zone in which timestamps in \emph{tstamp} are given. Will
#'   only be used if \emph{tstamp} are not yet of POSIXt-type.
#' @param tsep character used in ISO-8601 text representation to divide date
#'   from time
#'   
#' @return data frame with columns \emph{tPosix.local} (POSIXct-objects in time 
#'   zone \emph{tzone}), \emph{tzone} (name of time zone), \emph{tPosix.UTC} 
#'   (POSIXct-objects in time zone \emph{UTC}), \emph{tChr.ISO} (timestamp in 
#'   ISO-8601 syntax), \emph{isdst} (1 if daylight savings time is in effect, 
#'   otherwise 0).
#'   
#' @keywords internal
#' 
hsTsInfo <- function(tstamp, tzone = "", tsep = "T")
{  
  tPosix.local = as.POSIXct(tstamp, tz = tzone)
  
  tinfo <- data.frame(tPosix.local)
  tinfo$tzone <- format(tPosix.local, "%Z")    
  tinfo$tPosix.UTC <- as.POSIXct(format(tPosix.local, tz = "UTC"), tz = "UTC")
  
  # time difference to UTC in minutes
  minToUTC <- as.numeric(difftime(
    as.POSIXct(format(tinfo$tPosix.local), tz = "UTC"),
    as.POSIXct(format(tinfo$tPosix.UTC), tz = "UTC"), units = "min"
  ))
  
  formatstr <- paste0("%Y-%m-%d", tsep, "%H:%M:%S")
  
  tinfo$tChr.ISO <- sprintf(
    "%s%+03d:%02d", 
    format(tinfo$tPosix.local, format = formatstr), 
    as.integer(minToUTC / 60), # full hours of time difference to UTC
    minToUTC %% 60 # remaining minutes of time difference to UTC 
  )
  
  tinfo$isdst <- unclass(as.POSIXlt(tPosix.local))$isdst
  
  tinfo
}

# hsQua ------------------------------------------------------------------------

#' Number of Month to Number to Quarter
#' 
#' @param month number of month (1:12)
#'   
#' @return number of quarter: 1 if month in (1:3), 2 if month in (4:6), 3 if
#'   month in (7:9), 4 if month in (10:12)
#'
#' @keywords internal
#' 
hsQua <- function(month)
{
  as.integer((month - 1) / 3) + 1
}

# hsQuaStr ---------------------------------------------------------------------

#' Timestamp to Quarter String
#' 
#' @param tstamp (vector of) timestamp(s) of type POSXIXt
#'   
#' @return string of the form \dQuote{Q<n> of <yyyy>} with <n> representing the 
#'   number of the quarter within the year and <yyyy> representing the year.
#'   
#' @keywords internal
#' 
hsQuaStr <- function(tstamp) 
{
  quarter <- hsQua(as.integer(format(tstamp, "%m")))
  
  sprintf("Q%d of %s", quarter, format(tstamp, "%Y"))
}

# intervalKey ----------------------------------------------------------------

#' Representative String for Part of Timestamp
#' 
#' @param tstamps (vector of) timestamp(s) of type POSXIXt
#' @param itype one of 'y' (year), 'q' (quarter in year), 'm' (month in year),
#'   'd' (day in month in year), 'w' (week in year), 'D' (weekday in month in
#'   year), 'qo' (quarter only), 'mo' (month only), 'do' (day only), 'dm' (day
#'   in month), 'wo' (week only), 'Do' (weekday only), 'Dy' (weekday in year)
#'   
#' @export
#' 
#' @examples
#' # Define a sequence of times
#' times <- as.POSIXct(kwb.datetime::sequenceOfTimestamps(
#'   "2017-11-04 22:00:00", "2017-11-05 02:00:00", step.s = 3000
#' ))
#' 
#' # Apply all different defined types and print the result
#' for (type in rownames(kwb.datetime:::.timestamp_type_info())) {
#'   kwb.utils::printIf(TRUE, intervalKey(times, type), paste("\ntype:", type))
#' }
#' 
intervalKey <- function(tstamps, itype) 
{
  type_info <- .timestamp_type_info()
  
  if (! (itype %in% rownames(type_info))) {
    
    error_message <- paste0(
      "itype must be one of\n  ", 
      paste0(
        "'", rownames(type_info), "' (", type_info[, "desc"], ")", 
        collapse = ",\n  "
      )
    )
    
    stop(error_message)
  }
  
  if (itype == "q" || itype == "qo") {
    
    quarter <- hsQua(as.integer(format(tstamps, "%m")))

    if (itype == "q") {
      
      sprintf("%s-Q%d", format(tstamps, "%Y"), quarter)
      
    } else {
      
      sprintf("Q%d", quarter)
    }
  } else {
    
    format(tstamps, .intervalKeyFormat(itype))    
  }
}

# .timestamp_type_info ---------------------------------------------------------
.timestamp_type_info <- function()
{
  content <- "key, format, labelformat, desc
                y, %Y,                , year
                q, ,                  , quarter in year
                m, %Y-%m (%B %Y),     , month in year
                d, %Y-%m-%d,          , day in month in year
                w, %Y-W%U,            , week in year
                D, %Y-%m-WD%w (%A),   , weekday in month in year
               qo, ,                  , quarter only
               mo, %m (%B),           , month only
               do, %d,                , day only
               dm, %m-%d (%B %d),     , day in month
               wo, W%U (week %U),     , week only
               Do, WD%w (%A),         , weekday only
               Dy, %Y-WD%w (%A),      , weekday in year"
  
  result <- utils::read.table(text = content, sep = ",", header = TRUE)
  result <- do.call(cbind, lapply(result, kwb.utils::hsTrim))
  result[is.na(result)] <- ""
  
  rownames(result) <- result[, 1]
  
  result[, -1]
}

# .intervalKeyFormat -----------------------------------------------------------
.intervalKeyFormat <- function(itype)
{
  type_info  <- .timestamp_type_info()
  
  ids <- rownames(type_info)
  
  if (! (itype %in% ids)) {
    
    error_message <- paste(
      "itype must be one of", 
      paste0("'", ids, "' (", type_info[, "desc"], ")", collapse = ", ")
    )
    
    stop(error_message)
  }
  
  type_info[itype, 1]
}

# resetTimePart ----------------------------------------------------------------

#' Reduce a Timestamp to a certain Information
#' 
#' @param tstamp (vector of) timestamp(s) of type POSXIXt
#' @param resetYear logical. If \code{TRUE}, the year of all timestamps is set
#'   to 1970
#' @param resetMonth logical. If \code{TRUE}, the month of all timestamps is set
#'   to 01
#' @param resetDay logical. If \code{TRUE}, the day of all timestamps is set to
#'   01
#' @param resetTime logical. Used as default for \code{resetHour}, 
#'   \code{resetMin}, \code{resetSec}
#' @param resetHour logical. If \code{TRUE}, the hours of all timestamps is set
#'   to 00
#' @param resetMin logical. If \code{TRUE}, the minutes of all timestamps is set
#'   to 00
#' @param resetSec logical. If \code{TRUE}, the seconds of all timestamps is set
#'   to 00
#' @param \dots further arguments to be passed to hsToPosix
#' 
#' @keywords internal
#' @export
#'  
#' @examples
#' resetTimePart(as.POSIXct("2017-11-04 16:18:44"), resetTime = TRUE)
#' 
#' # Reset year, month and day
#' resetTimePart(as.POSIXct("2017-11-04 16:18:44"), TRUE, TRUE, TRUE)
#' 
resetTimePart <- function(
  tstamp, 
  resetYear  = FALSE, 
  resetMonth = FALSE, 
  resetDay   = FALSE,
  resetTime  = FALSE,
  resetHour  = resetTime,
  resetMin   = resetTime,
  resetSec   = resetTime,
  ...
) 
{
  fmt <- sprintf(
    "%s-%s-%s %s:%s:%s",
    ifelse(resetYear, "1970", "%Y"),
    ifelse(resetMonth,  "01", "%m"),
    ifelse(resetDay,    "01", "%d"),
    ifelse(resetHour,   "00", "%H"),
    ifelse(resetMin,    "00", "%M"),
    ifelse(resetSec,    "00", "%S")
  )
  
  hsToPosix(format(tstamp, fmt), ...)
}

# hsDateStr --------------------------------------------------------------------

#' Timestamp or Date Object to String in format yyyy-mm-dd
#'
#' @param tstamp vector of date or time objects  
#' 
#' @export
#' 
hsDateStr <- function(tstamp) 
{
  format(tstamp, "%Y-%m-%d")
}

# hsTStampsPerDay --------------------------------------------------------------

#' Number of Timestamps per Day
#' 
#' Number of timestamps per day in a vector of POSIX-time objects
#' 
#' @param tstamps vector of POSIX-timestamps
#' @param tsName name of "date" column in result data frame
#' 
#' @return data frame with columns \code{date} (or the name given in
#'   \code{tsName}) and \code{count}
#'   
#' @keywords internal
#' 
hsTStampsPerDay <- function(tstamps, tsName = "date")
{
  by <- list(hsDateStr(tstamps))
  
  result <- stats::aggregate(tstamps, by = by, FUN = length)
  
  structure(result, names = c(tsName, "count"))
}

# daysPerMonth ---------------------------------------------------------------

#' Number of Days in the Month of the given Date
#' 
#' @param date date object
#' 
#' @return (integer) number of days in the month of the given date
#' 
#' @export
#' 
#' @examples
#' # You may either pass a Date object...
#' daysPerMonth(as.Date("2010-01-04"))
#' 
#' # ... or a date string in yyyy-mm-dd format
#' daysPerMonth("2010-01-04")
#' 
#' # Number of days in February 2010
#' daysPerMonth("2010-02-01")
#' 
#' # Number of days in February 2012
#' daysPerMonth("2012-02-15")
#' 
daysPerMonth <- function(date) 
{
  date <- as.POSIXlt(date)
  
  date_string <- sprintf(
    "%04d-%02d-01", 
    ifelse(date$mon == 11, 1, 0) + date$year + 1900, # Dec? -> next year
    ifelse(date$mon == 11, 1, date$mon + 2) # Dec? -> Jan
  )
  
  as.integer(substr(as.Date(date_string) - 1, 9, 10))
}

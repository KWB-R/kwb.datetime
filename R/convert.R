#
# IMPORTANT fact about GMT/UTC timezones:
#
# http://stackoverflow.com/questions/7303580/understanding-the-etc-gmt-time-zone
#
# "For example, TZ='Etc/GMT+4' uses the abbreviation "GMT+4" and corresponds to
# 4 hours behind UTC (i.e. west of Greenwich) even though many people would
# expect it to mean 4 hours ahead of UTC (i.e. east of Greenwich)."
#

# iso_to_localtime ----------------------------------------------------------------

#' Text Timestamps to POSIXct
#' 
#' Convert text timestamps in a format according to ISO 8601 to POSIXct objects 
#' 
#' @param timestamps vector of character timestamps of format \code{yyyy-mm-dd
#'   HH:MM[:SS]<utc_offset>} with the seconds being optional and
#'   \code{<utc_offset>} being one of '+01' (UTC offset in Berlin in winter) or
#'   '+02' (UTC offset in Berlin in summer)
#' @param dbg if \code{TRUE} debug messages are shown
#' 
#' @export
#' 
#' @examples
#' times <- iso_to_localtime(c(
#'   "2017-10-29 01:00+02", 
#'   "2017-10-29 01:30+02", 
#'   "2017-10-29 02:00+02",
#'   "2017-10-29 02:30+02",
#'   "2017-10-29 02:00+01", 
#'   "2017-10-29 02:30+01", 
#'   "2017-10-29 03:00+01", 
#'   "2017-10-29 03:30+01"
#' ))
#' 
#' class(times)
#' plot(times, rep(1, length(times)), ylab = "", xlab = "LocalTime")
#' 
iso_to_localtime <- function(timestamps, dbg = TRUE)
{
  if (! is.character(timestamps)) {
    
    stop("timestamps are expected to be of mode character")
  }
  
  pattern <- timeFormatToRegex("%Y-%m-%d %H:%M")
  
  pattern <- paste0(pattern, "([:]\\d\\d)?[+-]\\d+$")
  
  if (! all(grepl(pattern, timestamps))) {
    
    stop(
      "Not all timestamps are in the expected format\n", 
      "  'yyyy-mm-dd HH:MM[:SS]<utc_offset>'\n", 
      "with the seconds being optional and <utc_offset> being one of '+01'", 
      "(UTC offset in winter) or '+02' (UTC offset in summer)!"
    )
  }
  
  localtimes <- as.POSIXct(rep(NA, length(timestamps)))
  
  in_winter <- grep("\\+01$", timestamps)
  in_summer <- grep("\\+02$", timestamps)
  
  (n_winter <- length(in_winter))
  (n_summer <- length(in_summer))

  # Find the length of the "pure" timestamp (without suffix "+01" or "+02")
  match_info <- regexec("^\\s*(.*)[+-]\\d+$", timestamps[1])[[1]]
  stop_index <- kwb.utils::getAttribute(match_info, "match.length")[2]
  
  # Extract the "pure" timestamp
  timestamps <- substr(timestamps, 1, stop_index)
  
  kwb.utils::catIf(dbg, "Converting", n_winter, "timestamps in CET ... ")
  localtimes[in_winter] <- as.POSIXct(timestamps[in_winter], tz = "Etc/GMT-1")
  kwb.utils::catIf(dbg, "ok.\n")
  
  kwb.utils::catIf(dbg, "Converting", n_summer, "timestamps in CEST ... ")
  localtimes[in_summer] <- as.POSIXct(timestamps[in_summer], tz = "Etc/GMT-2")
  kwb.utils::catIf(dbg, "ok.\n")
  
  localtimes
}

# decade -----------------------------------------------------------------------

#' Year Number to Decade Number
#' 
#' Convert a year number to a decade number (round down to full decade)
#' 
#' @param year year number, e.g. 2016
#' 
#' @keywords internal
#' 
decade <- function(year)
{
  stopifnot(is.numeric(year))
  
  10 * (year %/% 10)  
}

# to.GMT.plus.1 ----------------------------------------------------------------

#' Character Timestamps to POSIXct Objects (GMT+1)
#' 
#' @param timestamp character timestamp(s) to be converted to POSIXct in
#'   timezone "Etc/GMT+1"
#'   
#' @return vector of POSIXct in timezone \dQuote{Etc/GMT+1}
#' 
#' @export
#' 
to.GMT.plus.1 <- function(timestamp)
{
  stopifnot(is.character(timestamp))
  
  toGmtRelativePosix(timestamp, GMT.offset = 1)
}

# toGmtRelativePosix -----------------------------------------------------------

#' Character Timestamps to POSIXct Objects (GMT+\emph{offset})
#' 
#' Convert character timestamps to POSIXct objects in time zont 
#' GMT+\emph{offset}
#' 
#' @param timestamp vector of timestamps (character)
#' @param GMT.offset offset to GMT time. Default: 1 = Berlin Normal Time
#' @param format format string describing the format of \emph{timstamp}, see
#'   help for \code{strptime}. Default: "\%Y-\%m-\%d \%H:\%M:\%S"
#'
#' @export
#' 
toGmtRelativePosix <- function(timestamp, GMT.offset = 1, format = NULL)
{
  stopifnot(is.character(timestamp))

  # set default time format (it seems that inlinedocs does not like the default
  # assignment in the argument definition above)
  format <- kwb.utils::defaultIfNULL(format, "%Y-%m-%d %H:%M:%S")

  as.POSIXct(timestamp, tz = sprintf("Etc/GMT%+d", GMT.offset), format = format)
}

# stringToPosix ----------------------------------------------------------------

#' Convert a Time String to a POSIXct Object
#' 
#' Convert a time string to a POSIXct object. Allow for different possible 
#' timestamp formats.
#' 
#' @param x character vector of length one representing a timestamp
#' @param formats vector of allowed time formats (using \%-placeholders)
#' @param \dots arguments passed to \code{\link{hsToPosix}}
#' 
#' @export
#' 
#' @examples 
#' stringToPosix("2016-05-26")
#' stringToPosix("2016-05-26 12:00")
#'   
#' # additional arguments passed to hsToPosix
#' stringToPosix("2016-05-26 12:00:33", tzone = "ETC/Gmt-1")
#'   
#' # lt = TRUE -> create POSIXlt instead of POSIXct
#' lt1 <- stringToPosix("2016-05-26 17:00", lt = TRUE)
#' lt2 <- stringToPosix("2016-05-26 17:00", lt = TRUE, tz = "Europe/Berlin")
#'   
#' lt1$hour
#' lt1$isdst # normal time (is daylight saving time = FALSE)
#' lt2$isdst # summer time (is daylight saving time = TRUE)
#' 
stringToPosix <- function(
  x, formats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"), ...
)
{
  if (is.factor(x)) {
    
    x <- as.character(x)
  }
  
  if (class(x) != "character" || length(x) != 1) {
    
    stop("x must be a character vector of length one.")
  }
  
  # Find the matching format or stop
  isMatching <- sapply(formats, kwb.datetime::hasTimeFormat, timestamps = x)
  
  if (sum(isMatching) == 0) {

    formats <- kwb.utils::stringList(formats)
    
    stop("Timestamp '", x, "' does not match any of these formats: ", formats)
  }
  
  # Call the lubridate function corresponding to the matching pattern on x
  hsToPosix(x, format = formats[isMatching], ...)
}

# hsToPosix --------------------------------------------------------------------

#' Conversion to POSIXt
#' 
#' Converts an object representing a date (and if applicable a time) into an 
#' object of class POSIXct. Supported input classes are character, Date and 
#' POSIXt.
#' 
#' @param datetime object of class POSIXt or Date or character representing date
#'   (and time) information to be converted to class POSIXct.
#' @param keepTZ if \code{TRUE} and if the given object is already of
#'   POSIX-type, the returned POSIXct object will be in the same time zone as
#'   the original object. Otherwise POSIX-objects will be returned in the time
#'   zone \emph{tzone}.
#' @param tzone time zone. Will be set to \dQuote{UTC} if missing. UTC it the
#'   preferred time zone as it seems that only UTC prevents the POSIXt-classes
#'   from applying daylight-savings time.
#' @param lt if TRUE a POSIXlt object is returned instead of a POSIXct object.
#' @param \dots further arguments to be passed to as.POSIXct/as.POSIXlt, e.g.
#'   format, help for as.POSIXct/as.POSIXlt.
#' 
#' @details If \emph{datetime} is already of class POSIXlt or POSIXct the time
#'   zone is preserved unless \emph{keepTZ} is FALSE. If \emph{datetime} is a
#'   character string it is expected to be in ISO format: \dQuote{yyyy-mm-dd
#'   [HH:MM:SS]} where the time-part in brackets is optional.
#' 
#' @export
#' 
#' @examples 
#' # Start with a string representing a timestamp
#' datetime <- "2011-01-02 12:34:56"
#'   
#' # By default hsToPosix creates a POSIXct object:
#' ct <- hsToPosix(datetime)
#' class(ct) # "POSIXct" "POSIXt"
#'   
#' # You may decide to create a POSIXlt object instead:
#' lt <- hsToPosix(datetime, lt = TRUE)
#' class(lt) # "POSIXlt" "POSIXt"
#'   
#' # With a POSIXlt object you can access the different parts of the timestamp
#' sprintf("%d hours, %d minutes, %d seconds", lt$hour, lt$min, lt$sec)
#'   
#' # These are all available pieces of information 
#' # (isdst = is daylight savings time in effect)
#' sapply(attr(lt, "names"), function(part) lt[[part]])
#'   
#' # You may use hsToPosix to convert between lt and ct
#' identical(hsToPosix(ct, lt = TRUE), lt)
#' identical(hsToPosix(lt, lt = FALSE), ct)
#'   
#' # The following time does not exist in CET/CEST but in UTC 
#' # as it is the time when daylight-savings time switched.
#' hsToPosix("2011-03-27 02:00:00") # "2011-03-27 02:00:00 UTC"
#'   
#' # Compare with as.POSIXct: between 02:00:00 and 02:59:59 the 
#' # time information gets lost and is only recognized again 
#' # from 03:00:00 on. Similar results with as.POSIXlt.
#' as.POSIXlt("2011-03-27 01:59:59") # "2011-03-27 01:59:59"
#' as.POSIXlt("2011-03-27 02:00:00") # "2011-03-27"
#' as.POSIXlt("2011-03-27 02:59:59") # "2011-03-27"
#' as.POSIXlt("2011-03-27 03:00:00") # "2011-03-27 03:00:00"  
#'   
#' # When loading data from an Access table it will be of class
#' # POSIXct:
#' #dat <- hsGetTable(xmdb(), "tbl_Hyd")
#' #class(dat$Zeitst) # "POSIXct" "POSIXt"
#'   
#' # In order to prevent R from considering daylight savings time
#' # we should convert to UTC time zone. But then we have to keep
#' # in mind that the indication "UTC" is not correct as the time
#' # stamps in fact represent the time zone "UTC+1"!
#' #head(dat$Zeitst) 
#' # "2011-08-23 00:00:00 CEST" "2011-08-23 00:01:00 CEST" ...
#'   
#' #head(hsToPosix(dat$Zeitst))  
#' # "2011-08-23 00:00:00 UTC" "2011-08-23 00:01:00 UTC" ...
#' 
hsToPosix <- function(
  datetime, keepTZ = is.null(tzone), tzone = NULL, lt = FALSE, ...
) 
{
  # Check if object is of allowed class.
  allowed <- c("character", "Date", "POSIXt")
  
  isOfClass <- sapply(allowed, inherits, x = datetime)
  
  # Raise error if object is of neither of the supported classes.
  if (! any(isOfClass)) {
    
    stop(
      "datetime is not of one of the supported classes ", 
      kwb.utils::stringList(allowed)
    )
  }
  
  # Default time zone is Coordinated Universal Time (UTC)
  tzone <- kwb.utils::defaultIfNULL(tzone, "UTC")

  # Use as.POSIXct or as.POSIXlt as the conversion function
  functionName <- paste0("as.POSIX", ifelse(lt, "lt", "ct"))
  
  # If the object is of class POSIXt and the time zone is to be kept,
  # convert it to POSIXct (it may be in POSIXlt) and return the result
  if (inherits(datetime, "POSIXt") && keepTZ) {
    
    do.call(functionName, list(x = datetime))
    
  } else {
    
    # If the original object is of class Date or POSIXt it needs to be
    # transformed to a string containing the date in ISO format first.
    datetime <- as.character(datetime)
    
    # Return a POSIXct/POSIXlt object in the requested time zone.
    do.call(functionName, list(x = datetime, tz = tzone, ...))
  }
}

# .test_hsToPosix --------------------------------------------------------------
.test_hsToPosix <- function()
{
  datetime <- "2011-01-02 12:34:56"
  
  ct <- hsToPosix(datetime)
  lt <- hsToPosix(datetime, lt = TRUE)
  
  stopifnot("POSIXct" %in% class(ct))
  stopifnot("POSIXlt" %in% class(lt))
  
  stopifnot(identical(hsToPosix(ct), ct))
  stopifnot(identical(hsToPosix(lt), ct))
}

# berlinNormalTimeToUTC --------------------------------------------------------

#' berlinNormalTimeToUTC
#' 
#' @param x character string representing a timestamp measured in Berlin without
#'   adjusting time during the summer period, i.e. keeping the normal (= winter) 
#'   time (= UTC+1)
#' 
#' @keywords internal
#' 
berlinNormalTimeToUTC <- function(x) 
{
  localNormalTimeToUTC(x, UTCOffset = 1)
}

# berlinNormalTimeToBerlinLocalTime --------------------------------------------

#' berlinNormalTimeToBerlinLocalTime
#' 
#' @param x character string representing a timestamp measured in Berlin without
#'   adjusting time during the summer period, i.e. keeping the normal (= winter) 
#'   time (= UTC+1)
#'   
#' @keywords internal
#' 
berlinNormalTimeToBerlinLocalTime <- function(x) 
{
  localNormalTimeToLocalTime(x, UTCOffset = 1, tz = "Europe/Berlin")
}

# utcToBerlinLocalTime ---------------------------------------------------------

#' utcToBerlinLocalTime
#' 
#' @param x Character timestamp that is interpretable by
#'   \code{\link{as.POSIXct}}, representing a time given in UTC
#' 
#' @keywords internal
#' 
utcToBerlinLocalTime <- function(x) 
{
  utcToLocalTime(x, "Europe/Berlin")
}

# localNormalTimeToLocalTime ---------------------------------------------------

#' localNormalTimeToLocalTime
#' 
#' @param x character timestamp that is interpretable by
#'   \code{\link{as.POSIXct}}, representing a time given in UTC
#' @param UTCOffset UTC offset in number of hours that the local normal time is 
#'   ahead of UTC, e.g. +01 hour for Berlin nomral (= winter) time 
#' @param tz time zone of the local time
#' 
#' @keywords internal
#' 
localNormalTimeToLocalTime <- function(x, UTCOffset, tz)
{
  utcToLocalTime(localNormalTimeToUTC(x, UTCOffset), tz)
}

# localNormalTimeToUTC ---------------------------------------------------------

#' localNormalTimeToUTC
#' 
#' @param x Character timestamp that is interpretable by
#'   \code{\link{as.POSIXct}}, representing a time given in UTC
#' @param UTCOffset UTC offset in number of hours that the local normal time is 
#'   ahead of UTC, e.g. +01 hour for Berlin nomral (= winter) time 
#' 
#' @keywords internal
#' 
localNormalTimeToUTC <- function(x, UTCOffset) 
{
  stopifnot(is.character(x))
  
  format(as.POSIXct(x, tz = "UTC") - UTCOffset * 3600, tz = "UTC")  
}

# utcToLocalTime ---------------------------------------------------------------

#' utcToLocalTime
#' 
#' @param x Character timestamp that is interpretable by
#'   \code{\link{as.POSIXct}}, representing a time given in UTC
#' @param tz string representing the timezone to which the timestamp is to be
#'   converted
#'
#' @keywords internal
#' 
utcToLocalTime  <- function(x, tz) 
{
  stopifnot(is.character(x))  
  
  format(as.POSIXct(x, tz = "UTC"), tz = tz)  
}

# utcOffset --------------------------------------------------------------------

#' Get UTC Offset from Local and UTC Timestamp (Character)
#' 
#' @param LocalDateTime character string representing a local timestamp
#' @param DateTimeUTC character string representing a time stamp in UTC
#' 
#' @keywords internal
#' 
utcOffset <- function(LocalDateTime, DateTimeUTC)
{
  stopifnot(is.character(LocalDateTime) && is.character(DateTimeUTC))
  
  as.numeric(difftime(
    as.POSIXct(LocalDateTime, tz = "UTC"),
    as.POSIXct(DateTimeUTC, tz = "UTC"), 
    units = "h"
  ))
}

# # berlinWinterTimeToBerlinLocalTime --------------------------------------------
# berlinWinterTimeToBerlinLocalTime <- function # berlinWinterTimeToBerlinLocalTime
# ### berlinWinterTimeToBerlinLocalTime
# (
#   x
#   ### character timestamp given in Berlin winter time (i.e. the local timestamp
#   ### itself for times in winter and the local timestamp, reduced by one hour
#   ### for times in summer)
# )
# {
#   utcToBerlinLocalTime(berlinWinterTimeToUTC(x)$charUTC)
# }
# 
# # utcToLocalTimeAndOffset ------------------------------------------------------
# utcToLocalTimeAndOffset <- function # UTC timestamp to Local Time in time zone
# ### convert UTC timestamp to Local Time (in given time zone)
# (
#   DateTimeUTC,
#   ### character timestamp given in UTC
#   tz = "Europe/Berlin",
#   ### timezone. Default: "Europe/Berlin"
#   stringsAsFactors = FALSE
# )
# {
#   stopifnot(is.character(x))
#   
#   utc <- "UTC"
#   LocalDateTime <- format(as.POSIXct(x, tz=utc), tz=tz)  
#   UTCOffset <- as.numeric(difftime(as.POSIXct(LocalDateTime, tz=utc),
#                                    as.POSIXct(DateTimeUTC, tz=utc), units="h"))
#   
#   data.frame(LocalDateTime = LocalDateTime,        
#              UTCOffset = UTCOffset,
#              DateTimeUTC = DateTimeUTC)
#   ### data frame with character (or factor, if stringsAsFactors = TRUT) columns
#   ### \code{LocalDateTime}, \code{UTCOffset}, \code{DateTimeUTC}
# }
# 
# # utcToBerlinLocalTime ---------------------------------------------------------
# utcToBerlinLocalTime <- function # UTC timestamp to Berlin Local Time
# ### convert UTC timestamp to Berlin Local Time (including switches of Daylight
# ### Saving Time)
# (
#   x
#   ### character timestamp given in UTC
# )
# {
#   utcToLocalTime(x, "Europe/Berlin")
#   ### list with elements \code{charLocal}, \code{posixLocal}, \code{utcOffset}
# }
# 
# # utcToOdmDateTimeColumns ------------------------------------------------------
# utcToOdmDateTimeColumns <- function # UTC timestamps to ODM DateTime-columns
# ### UTC timestamps to ODM DateTime-columns
# (
#   DateTimeUTC,
#   ### character timestamps representing UTC time
#   tz = "Europe/Berlin",
#   ### timezone. Default: "Europe/Berlin"
#   stringsAsFactors = FALSE
# )
# {
#   stopifnot(is.character(x))
#   
#   
#   data.frame(LocalDateTime = LocalDateTime,
#              UTCOffset = UTCOffset,
#              DateTimeUTC = DateTimeUTC, 
#              stringsAsFactors = stringsAsFactors)  
# }
# 
# # berlinWinterTimeToUTC --------------------------------------------------------
# berlinWinterTimeToUTC <- function # berlinWinterTimeToUTC
# ### berlinWinterTimeToUTC
# (
#   x
#   ### character timestamp given in Berlin winter time (i.e. the local timestamp
#   ### itself for times in winter and the local timestamp, reduced by one hour
#   ### for times in summer)
# )
# {
#   stopifnot(is.character(x))  
#   
#   offsetUTC <- 1
#   tz <- "UTC"
#   posixUTC <- as.POSIXct(x, tz=tz) - offsetUTC * 3600
#   charUTC <- format(posixUTC, tz=tz)  
#   
#   list(charUTC=charUTC, posixUTC=posixUTC)
#   ### list with elements \code{charUTC}, \code{posixUTC}
# }
# 

# toUTC ------------------------------------------------------------------------

#' Convert POSIXt Object to UTC Time Zone
#' 
#' @param x object of class POSIXt (either POSIXct or POSIXlt)
#' 
#' @return POSIXt object in UTC timezone (hopefully!)
#' 
#' @export
#' 
#' @examples 
#' 
#' # Create a timestamp in the time zone that is set on the local machine
#' time <- as.POSIXct("2017-11-01 01:16")
#' 
#' # Convert time zone to UTC
#' time_utc <- toUTC(time)
#' 
#' # The new time zone "UTC" is set in the attribute "tz"
#' attr(time_utc, "tz")
#' 
#' # The times mean the same, just expressed in another time zone!
#' time_utc == time
#' 
toUTC <- function(x)
{
  toTimezone(x, "UTC")
}

# toTimezone -------------------------------------------------------------------

#' Convert POSIXt Object to given Time Zone
#' 
#' @param x object of class POSIXt (either POSIXct or POSIXlt)
#' @param tz timezone, e.g. "Europe/Berlin", see \code{base::timezones} for help
#'   about possible time zone strings
#'   
#' @return POSIXt object in requested timezone (hopefully!)
#'   
#' @keywords internal
#' 
toTimezone <- function(x, tz)
{
  stopifnot(inherits(x, "POSIXt"))
  
  as.POSIXct(format(x, tz = tz), tz = tz)  
}

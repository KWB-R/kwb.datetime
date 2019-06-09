# matchingTimeFormat -----------------------------------------------------------

#' Find Time Format matching a Timestamp
#' 
#' @param timestamp character timestamp to be checked against different possible
#'   timstamp formats
#' @param timeFormats vector of possible time formats with placeholders %Y
#'   (year), %m (month), %d (day), %H (hours), %M (minutes), %S (seconds) as
#'   described for \code{format.POSIXct}
#' @param method passed to \code{\link{hasTimeFormat}}
#' @param warn if \code{TRUE} an R warning is issued if no matching format was
#'   found. Otherwise a message is given.
#' @param failureValue value returned in case that no matching format was found.
#'   Default: \code{NULL}
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
matchingTimeFormat <- function(
  timestamp, timeFormats = getTimeFormats(), method = 1L, warn = TRUE, 
  failureValue = NULL
)
{
  if (! is.character(timestamp)) stop(
    "matchingTimeFormat() expects a (vector of) character as input",
    call. = FALSE
  )
  
  if (length(timestamp) > 1) return(unlist(lapply(
    timestamp, matchingTimeFormat, timeFormats = timeFormats, method = method,
    warn = warn, failureValue = NA
  )))
  
  # Helper functions
  format_enum_string <- function(x) kwb.utils::stringList(x, collapse = ",\n  ")
  warn_or_message <- function(warn, msg) {
    if (warn) warning(msg, call. = FALSE) else message(msg)
  }
  
  ok <- which(sapply(
    timeFormats, hasTimeFormat, timestamps = timestamp, method = method,
    USE.NAMES = FALSE
  ))
  
  n <- length(ok)
  
  if (n == 0) {
    
    warn_or_message(warn, sprintf(
      "No matching format found for timestamp '%s'. Considered formats:\n  %s", 
      timestamp, format_enum_string(timeFormats)
    ))
    
    return(failureValue)
  }
  
  if (n == 1) {
    return(timeFormats[ok])
  }
  
  warn_or_message(warn, sprintf(
    "Found more than one matching format for '%s':\n  %s", 
    timestamp, format_enum_string(timeFormats[ok])
  ))
  
  # Return the format with the longest format string
  timeFormats[ok][which.max(nchar(timeFormats[ok]))]
}

# getTimeFormats ---------------------------------------------------------------
getTimeFormats <- function()
{
  date_formats <- c(
    "%Y-%m-%d", "%y-%m-%d", "%d.%m.%Y", "%d.%m.%y", "%d/%m/%Y", "%d/%m/%y",
    "%m/%d/%Y", "%m/%d/%y", "%d. %B %Y"
  )
  
  combinations <- kwb.utils::expandGrid(
    date_formats = date_formats, 
    time_formats = c("%H:%M:%S", "%H:%M")
  )
  
  c(
    date_formats,
    kwb.utils::pasteColumns(combinations),
    kwb.utils::pasteColumns(combinations, rev(names(combinations)))
  )
}

# hasTimeFormat ----------------------------------------------------------------

#' Do Timestamps have the expected Format?
#' 
#' Checks if timestamps are formatted according to timeformat
#' 
#' @param timestamps character strings representing timestamps.
#' @param timeformat character string giving a date-time format as used by 
#'   \code{\link{strptime}}.
#' @param method Method used to do the check. 1: Check based on pattern matching
#'   2: Check whether \code{\link{as.POSIXct}} returns valid time objects and if
#'   re-formatting these objects are identical to the original timestamps. The
#'   simple check should be much faster for many timestamps to be checked.
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
hasTimeFormat <- function(timestamps, timeformat = NULL, method = 1L)
{
  # Set default time format (it seems that inlinedocs does not like the default
  # assignment in the argument definition above)
  timeformat <- kwb.utils::defaultIfNULL(timeformat, "%Y-%m-%d %H:%M:%S")
  
  if (method == 1L) {
    
    pattern <- sprintf("^%s$", timeFormatToRegex(timeformat))
    grepl(pattern, timestamps)
    
  } else if (method == 2L) {
    
    posix_time <- as.POSIXct(timestamps, tz = "UTC", format = timeformat)
    ! is.na(posix_time) && 
      identical(timestamps, format(posix_time, format = timeformat))
    
  } else {
    
    stop("No such method. Use method = 1 (pattern matching) or method = 2 ", 
         "(identity of converting back and forth)", call. = FALSE)
  }
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


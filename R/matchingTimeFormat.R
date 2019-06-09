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

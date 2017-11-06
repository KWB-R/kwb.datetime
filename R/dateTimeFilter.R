# hsTsBefore -------------------------------------------------------------------

#' Timestamps before Timestamp Limit?
#' 
#' Returns a vector of booleans indicating whether the timestamps at 
#' corresponding positions in a vector of timestamps lie before a given 
#' timestamp.
#' 
#' @param tstamps vector of timestamps, either as character strings in
#'   ISO-format (yyyy-mm-dd HH:MM:SS), as Date objects or as POSIXt-objects
#' @param tsLimit last timestamp (\code{limIncluded} == TRUE) or timestamp
#'   directly after last timestamp (\code{limIncluded} == FALSE) to be
#'   considered
#' @param limIncluded if \code{TRUE}, timestamps that are equal to 
#'   \code{tsLimit} are considered to be contained in the result set
#'   
#' @keywords internal
#' 
hsTsBefore <- function(tstamps, tsLimit, limIncluded = TRUE)
{
  timestampIn(
    tstamps = tstamps, 
    tsFirst = NULL, 
    tsLast = tsLimit, 
    firstIncluded = NULL, 
    lastIncluded = limIncluded
  )
}

# hsTsAfter --------------------------------------------------------------------

#' Timestamps after Timestamp Limit?
#' 
#' Returns a vector of booleans indicating whether the timestamps at 
#' corresponding positions in a vector of timestamps lie after a given 
#' timestamp.
#' 
#' @param tstamps vector of timestamps, either as character strings in
#'   ISO-format (yyyy-mm-dd HH:MM:SS), as Date objects or as POSIXt-objects
#' @param tsLimit first timestamp (\code{limIncluded} == TRUE) or timestamp
#'   directly before first timestamp (\code{limIncluded} == FALSE) to be
#'   considered
#' @param limIncluded if \code{TRUE}, timestamps that are equal to 
#'   \code{tsLimit} are considered to be contained in the result set
#'
#' @keywords internal
#' 
hsTsAfter <- function(tstamps, tsLimit, limIncluded = TRUE) 
{
  timestampIn(
    tstamps = tstamps, 
    tsFirst = tsLimit, 
    tsLast = NULL, 
    firstIncluded = limIncluded, 
    lastIncluded = NULL
  )
}

# hsTsIn -----------------------------------------------------------------------

#' Deprecated use \code{\link{timestampIn}} instead
#' 
#' @param ... passed to \code{\link{timestampIn}}
#' 
#' @export
#' 
hsTsIn <- function(...)
{
  kwb.utils::.warningDeprecated("hsTsIn", "timestampIn")
  timestampIn(...)
} 

# timestampIn ------------------------------------------------------------------

#' Timestamps within Time Interval?
#' 
#' Returns vector of booleans indicating whether the timestamps at corresponding
#' positions in a vector of timestamps lie within a time interval, specified by 
#' first and/or last timestamp of the interval.
#' 
#' @param tstamps vector of timestamps, either as character strings in 
#'   ISO-format (yyyy-mm-dd HH:MM:SS), as Date objects or as POSIXt-objects
#' @param tsFirst first timestamp (\code{firstIncluded} == TRUE) or timestamp 
#'   directly before first timestamp (\code{firstIncluded} == FALSE) to be 
#'   considered
#' @param tsLast last timestamp (\code{lastIncluded} == TRUE) or timestamp 
#'   directly after last timestamp (\code{lastIncluded} == FALSE) to be 
#'   considered
#' @param firstIncluded if \code{TRUE}, \code{tsFirst} represents the first,
#'   otherwise the timestamp direcly before the first timestamp to be
#'   considered.
#' @param lastIncluded if \code{TRUE}, \code{tsLast} represents the last,
#'   otherwise the timestamp direcly after the last timestamp to be considered.
#' @param dbg should debug messages be shown?
#' 
#' @export
#' 
#' @examples
#' tstamps <- sequenceOfTimestamps("2017-11-03", "2017-11-04")
#' table(timestampIn(tstamps, "2017-11-03 12:00:00", "2017-11-03 13:00:00"))
#' 
timestampIn <- function(
  tstamps, tsFirst = NULL, tsLast = NULL, firstIncluded = TRUE, 
  lastIncluded = FALSE, dbg = FALSE
) 
{
  # Prepare vector of TRUE values
  isIn <- rep(TRUE, length(tstamps))
  
  # If there are no limits, return vector of TRUE values
  if (is.null(tsFirst) && is.null(tsLast)) {
    
    return (isIn)
  }
  
  # If needed, convert timestamps (from character, Date) to POSIX-type
  if (! ("POSIXt" %in% class(tstamps))) {
    
    tstamps <- hsToPosix(tstamps)    
  }
  
  for (first in c(TRUE, FALSE)) {

    # If tsFirst (tsLast) is given set isIn vector at positions of timestamps 
    # before tsFirst (after tsLast) to FALSE
    
    limit <- ifelse(first, tsFirst, tsLast)
    included <- ifelse(first, firstIncluded, lastIncluded)
    
    if (! is.null(limit)) {
      
      if (! ("POSIXt" %in% class(limit))) {
        
        limit <- hsToPosix(limit)    
      }
      
      kwb.utils::catIf(dbg, sprintf(
        "looking for timestamps %s %s%s...\n", 
        ifelse(first, "after", "before"),
        ifelse(included,  "or at ", ""),
        format.Date(limit)
      ))
      
      isIn <- isIn & if (first) {
        
        if (included) tstamps >= limit else tstamps > limit
        
      } else {
        
        if (included) tstamps <= limit else tstamps < limit
      }    
    }
  }
  
  isIn
}

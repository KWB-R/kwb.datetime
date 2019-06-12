# textToEuropeBerlinPosix ------------------------------------------------------

#' Convert Text Timestamps to POSIXct Objects
#' 
#' This function tries to overcome some problems that may arise when using
#' \code{\link{as.POSIXct}}. It can handle timestamps that originate from a 
#' clock that switches between standard time and summer time as well as those 
#' originating from a clock that stays in standard time over the whole year.  
#' See \code{vignette("text_to_posixct", package = "kwb.datetime")} for details.
#' It also tries to find a convenient format description string. 
#' 
#' When reading timestamps that observe Daylight Saving, it is required  that 
#' the timestamps in \code{x} are ordered by time, which should be the case if 
#' they  were recorded by a measuring device.
#' 
#' @param x vector of text (i.e. character) timestamps
#' @param format format string describing the format of a timestamp, such as
#'   "%Y-%m-%d %H:%M:%S", see \code{\link{strftime}} for further percentage sign
#'   placeholders. If not given or \code{NULL}, the function tries to guess the
#'   format from the first timestamp given in \code{x}.
#' @param switches if \code{TRUE} (the default), the timestamps are assumed to
#'   originate from a clock that switches between standard time and summer time.
#'   Otherwise (\code{switches = FALSE}) timestamps are assumed to originate 
#'   from a clock that stays in standard time over the whole year.
#' @param dbg if \code{TRUE} debug messages are shown
#' @return vector of POSIXct objects
#' @export
#' @examples 
#' # Test the functions with the following "switch" days
#' kwb.datetime::date_range_CEST(2019)
#' 
#' t1 <- textToEuropeBerlinPosix(c("31.03.2019 01:00", "31.03.2019 03:00"))
#' t2 <- textToEuropeBerlinPosix(c("31.03.2019 01:00", "31.03.2019 02:00"), 
#'                               switches = FALSE)
#' identical(t1, t2)
#'                               
#' t3 <- textToEuropeBerlinPosix(c("27.10.2019 02:00", "27.10.2019 02:00"))
#' t4 <- textToEuropeBerlinPosix(c("27.10.2019 01:00", "27.10.2019 02:00"), 
#'                               switches = FALSE)
#' identical(t3, t4)
#' 
#' kwb.datetime::textToEuropeBerlinPosix(c(
#'   "2017-10-29 01:30:00", # 1: CEST
#'   "2017-10-29 02:00:00", # 2: CEST
#'   "2017-10-29 02:30:00", # 3: CEST
#'   "2017-10-29 02:00:00", # 4: CET
#'   "2017-10-29 02:30:00", # 5: CET
#'   "2017-10-29 03:00:00"  # 6: CET
#' ))
#' 
textToEuropeBerlinPosix <- function(
  x, format = NULL, switches = TRUE, dbg = TRUE
)
{
  stopifnot(is.character(x))

  if (is.null(format)) {
    
    stopifnot(length(x) > 0)
    
    format <- kwb.utils::catAndRun(dbg = dbg, "Guessing time format", {
      matchingTimeFormat(x[1])
    })
  }
  
  result <- kwb.utils::catAndRun(
    dbg = dbg, sprintf("Converting %d timestamps to POSIXct", length(x)), {

    # Determine and append UTC offsets if not yet given in the timestamps
    if (! grepl("%z", format)) {
      
      # Determine the UTC offsets of the timestamps
      if (! switches) {
        
        utc_offset <- rep(1L, length(x))
        
      } else {
        
        utc_offset <- try(silent = TRUE, {
          utcOffsetBerlinTime(reformatTimestamp(x, old.format = format))
        })
        
        if (inherits(utc_offset, "try-error")) stop(
          call. = FALSE, attr(utc_offset, "condition")$message, 
          "\nConsider setting keepStandardTime to TRUE!"
        )
      } 
      
      # Append the UTC offsets to the timestamps
      x <- paste0(x, sprintf("%+03d00", utc_offset))
      
      # Append the placeholder %z indicating UTC offset to the format string
      format <- paste0(format, "%z")
    }
    
    # Explicitly set the time zone
    as.POSIXct(x, tz = "Europe/Berlin", format = format)
  })
  
  result
}

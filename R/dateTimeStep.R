# roundTime --------------------------------------------------------------------

#' Timestamp complying with Time Step
#' 
#' Returns for (each of) the given timestamp(s) the timestamp(s) itself if it
#' represents a multiple of the given time step or the nearest smaller or
#' nearest greater timestamp that represents a multiple of the time step.
#' 
#' @param tstamp (vector of) timestamp(s) of class "POSIXlt" or "POSIXct"
#' @param tstep time step in seconds of which timestamps in \emph{tstamp} shall
#'   represent multiples
#' @param direction one of -1, 0, 1. If -1, the nearest timestamp (either
#'   smaller or greater) complying with the timestamp is returned. If 0, always
#'   the nearest greater timestamp and if 1, always the nearest smaller
#'   timestamp is returned.
#'   
#' @return (Vector of) timestamp(s) corresponding to timestamp(s) given in 
#'   \emph{tstamp} being \dQuote{rounded} to the nearest --- greater or smaller 
#'   (direction == -1), always smaller (direction == 1) or always greater 
#'   (direction == 0) --- timestamp representing a multiple of the given time
#'   step \emph{tstep}.
#'
#' @export
#' 
#' @examples 
#' # Generate a timestamp to be "rounded"
#' t0 <- hsToPosix("2011-12-24 18:22:05")
#'   
#' # Round to nearest (default) full minute 
#' roundTime(t0, 60) ## = 2011-12-24 18:22:00 UTC
#'   
#' # Round to nearest full greater minute
#' roundTime(t0, 60, 0) ## = 2011-12-24 18:23:00 UTC
#'   
#' # Round to nearest multiple of 15 minutes (-1 could be omitted)
#' roundTime(t0, 15*60, -1) ## 2011-12-24 18:15:00 UTC
#'   
#' # Round to nearest smaller multiple of four hours
#' roundTime(t0, 4*60*60, 1) ## 2011-12-24 16:00:00 UTC
#' 
roundTime <- function(tstamp, tstep, direction = -1) 
{
  if (missing(tstamp)) {
    stop("Please give (a vector of) timestamp(s) of class POSIXlt or POSIXct.\n")
  }
  
  if (missing(tstep)) {
    stop(paste(
      "Please specify a time step in seconds that the timestamps shall ", 
      "represent multiples of.\n"
    ))
  }
  
  if (! direction %in% c(-1, 0, 1)) {
    stop("direction must be one of -1 (nearest), 0 (greater) or 1 (smaller).\n")
  }
  
  ## Calculate rest(s) of dividing the timestamps (in seconds since origin) 
  ## by the time step
  rest <- as.integer(tstamp) %% tstep
  
  if (direction == -1) { 
    
    # "round" to nearest time step multiple
    # sel ("selected") is a vector of booleans being TRUE at positions of 
    # timestamps for which the rest is less than or equal to tstep/2
    sel <- (rest <= tstep / 2) 
    tstamp[ sel] <- tstamp[ sel]          - rest[ sel]
    tstamp[!sel] <- tstamp[!sel] + (tstep - rest[!sel])
    
  } else {
    
    # sel ("selected") is a vector of booleans being TRUE at positions of 
    # timestamps for which the rest is greater than 0
    sel <- (rest > 0) 
    
    if (direction == 1) {
      
      # "round" to time step multiple smaller than timestamp
      tstamp[sel] <- tstamp[sel] - rest[sel]
      
    } else {
      
      # "round" to time step multiple greater than timestamp
      tstamp[sel] <- tstamp[sel] + (tstep - rest[sel])
    }      
  }
  
  return (tstamp)
}

# minTimeStep ------------------------------------------------------------------

#' Minimum Time Step in Sequence of Timestamps
#' 
#' @param tstamps vector of POSIX-type timestamps or any other vector that can
#'   be converted to integer
#' @param dbg should debug messages be shown?
#'
#' @export
#' 
#' @examples
#' tstamps <- seq(as.POSIXct("2017-11-03"), as.POSIXct("2017-11-04"), 3600)
#' minTimeStep(tstamps)
#' 
#' # No need for timestamps!
#' minTimeStep(c(10, 20, 30, 40, 45, 50, 60))
#' minTimeStep(c(10, 20, 30, 40, 45, 50, 60), dbg = TRUE)
#' 
minTimeStep <- function(tstamps, dbg = FALSE)
{
  n <- length(tstamps)
  
  if (n < 2) {
    
    stop("At least two timestamps needed, tstamps has only", n)
  }
  
  ## Convert to vector of integers
  tstamps <- as.integer(tstamps)
  
  ## Find differences by shifting elements by one
  tstep <- min(diff(tstamps), na.rm = TRUE)
  
  if (tstep == 0){
    
    stop("There are duplicate timestamps in tstamps!")
  }
  
  kwb.utils::catIf(dbg, sprintf(
    "timestep: %d s (%s).\n", tstep, 
    "min. time diff. found between consecutive timestamps"
  ))
  
  tstep
}

# getTimestepInSeconds ---------------------------------------------------------

#' Get Time Step in Seconds
#' 
#' Find the time step applied in a sequence of timestamps. Give a warning if 
#' more than one time step was found.
#'
#' @param timestamps vector of POSIXt objects
#' @param default default time step in seconds. Default: 60
#'   
#' @return smallest, non-zero timestep in seconds found in the sequence of
#'   timestamps
#'
#' @export
#' 
getTimestepInSeconds <- function(timestamps, default = 60)
{
  stopifnot("POSIXt" %in% class(timestamps))
  
  if (length(timestamps) < 2) {
    
    timestep <- default
    
    warning(
      "Cannot calculate timestep from zero or one timestamp.",
      " I will take the default:", default, "seconds."
    )
  } else {
    
    # Generate a warning if the time step is not unique
    isValidTimestampSequence(timestamps, checks = c("timestep", "sorted"))
    
    timeDifferences <- sort(unique(diff(as.double(timestamps))))
    
    timestep <- timeDifferences[timeDifferences > 0][1]    
  }  
  
  return (timestep)  
}

# isValidTimestampSequence -----------------------------------------------------

#' Check Sequence of Timestamps for Validity
#' 
#' Different validation checks for sequence of timestamps
#' 
#' @param timestamps vector of timestamps of class POSIXt
#' @param checks Vector of character indicating the checks to be performed.
#'   Available checks: "sorted", "duplicates", "timestep". Default: perform all
#'   tests
#' @param dbg shall debug messages be shown?
#' 
#' @export
#' 
#' @examples
#' timestamps <- sequenceOfTimestamps("2017-11-03", "2017-11-04", 3600)
#' times <- as.POSIXct(timestamps)
#' isValidTimestampSequence(times)
#' isValidTimestampSequence(rev(times))
#' isValidTimestampSequence(timestamps = c(times[1], times))
#' 
isValidTimestampSequence <- function(
  timestamps, checks = c("sorted", "duplicates", "timestep"), dbg = FALSE
)
{
  stopifnot("POSIXt" %in% class(timestamps))
  
  messages <- c()
  
  # timestamps must be sorted
  
  if ("sorted" %in% checks) {
    
    kwb.utils::catIf(dbg, "Checking if timestamps are sorted...\n")
    
    timestamps.sorted <- sort(timestamps)
    
    if (! all(timestamps == timestamps.sorted)) {
      
      messages <- c(messages, "The timestamps are not sorted!")
    }
  }
  
  # time step between timestamps must be constant
  
  if ("timestep" %in% checks) {
    
    kwb.utils::catIf(dbg, "Checking if the time step is unique...\n")
    
    timeDifferences <- diff(timestamps)
    uniqueTimeDifferences <- unique(timeDifferences)
    
    if (length(uniqueTimeDifferences) > 1) {
      
      thisMessage <- sprintf(
        "The time step between timestamps is not unique: %s (%s)",
        paste(utils::head(uniqueTimeDifferences), collapse = ", "),
        attr(timeDifferences, "unit")
      )
      
      messages <- c(messages, thisMessage)
    }
  }
  
  # timestamps must not contain duplicates
  
  if ("duplicates" %in% checks) {
    
    kwb.utils::catIf(dbg, "Checking if there are duplicate timestamps...\n")
    
    frequency <- table(timestamps)
    n_duplicates <- sum(frequency > 1)
    
    if (n_duplicates) {
      
      thisMessage <- paste(
        "The timestamps contain", n_duplicates, "duplicates such as:\n",
        kwb.utils::stringList(utils::head(names(frequency)[frequency > 1]))
      )
      
      messages <- c(messages, thisMessage)
    }
  }
  
  if (length(messages) > 0) {
    
    warning(paste("\n***", messages, collapse = "\n"))
    return (FALSE)    
  }
  
  return (TRUE)
}

# sequenceOfTimestamps ---------------------------------------------------------

#' Create a Sequence of Timestamps
#' 
#' Creates timestamps of mode character between first timestamp \emph{from} and
#' \emph{to} with a distance of \emph{step.s} seconds between the timestamps.
#' 
#' @param from first timestamp of mode character in ISO-Syntax: yyyy-mm-dd
#'   [HH:MM:SS] where the part in brackets is optional.
#' @param to last timestamp of  mode character in ISO-Syntax: yyyy-mm-dd
#'   [HH:MM:SS] where the part in brackets is optional.
#' @param step.s time step between the timestamps in seconds.
#'   
#' @return Vector of character timestamps
#'   
#' @export
#' 
#' @examples 
#' # Create timestamps of January 2011 with five minutes step
#' sequenceOfTimestamps("2011-01-01 19:00:00", "2011-01-02", 300)
#'
sequenceOfTimestamps <- function(from, to, step.s = 60)
{
  stopifnot(is.character(from) && is.character(to))
  
  # Create sequence of POSIXct timestamps in UTC
  tstamps = seq(
    as.POSIXct(from, tz = "UTC"), as.POSIXct(to, tz = "UTC"), by = step.s
  )
  
  # Return vector of timestamps
  format(tstamps, tz = "UTC")
}

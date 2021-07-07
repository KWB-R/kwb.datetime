# test_TimeConversionFunctions -------------------------------------------------

#' Test Time Conversion Functions
#' 
#' @param year year for which to demonstrate the switch between Central European
#'    Time (CET) and Central European Summer Time (CEST)
#' @param normalToSummer \code{TRUE}: CET to CEST, \code{FALSE}: CEST to CET
#' 
test_TimeConversionFunctions <- function(year = 2000, normalToSummer = TRUE)
{
  times <- timesAroundClockChange(year, normalToSummer)
  
  chrUTC <- format(times, tz = "UTC")
  
  cat("UTC to Berlin Local Time:\n")
  x <- data.frame(chrUTC = chrUTC, stringsAsFactors = FALSE)
  
  #berlinLocal <- utcToBerlinLocalTime(x$chrUTC)
  #x$chrBerlinLocal <- berlinLocal$charLocal  
  #x$posixBerlinLocal <- berlinLocal$posixLocal

  x$chrBerlinLocal <- utcToBerlinLocalTime(x$chrUTC)  
  x$posixBerlinLocal <- as.POSIXct(x$chrBerlinLocal, tz = "Europe/Berlin")  
  x$intBerlinLocal <- as.integer(x$posixBerlinLocal)
  
  x$dt <- c(NA, diff(x$intBerlinLocal))
  x$chrTimezone <- format(x$posixBerlinLocal, "%Z")
  x$DST <- daylightSavingTimeInEffect(x$posixBerlinLocal)
  #x$numUTCOffset <- berlinLocal$utcOffset
  x$numUTCOffset <- utcOffset(x$chrBerlinLocal, chrUTC)
  
  x$backToUTC <- toUTC(x$posixBerlinLocal)
  x1 <- x
  print(x1)
  
  cat("\n\nBerlin Normal (Winter) Time to Berlin Local Time:\n")
  x <- data.frame(chrBerlinNormal = chrUTC, stringsAsFactors = FALSE)
  
  #berlinLocal <- berlinWinterTimeToBerlinLocalTime(x$chrBerlinNormal)
  berlinLocal <- berlinNormalTimeToBerlinLocalTime(x$chrBerlinNormal)
  
  #x$chrBerlinLocal <- berlinLocal$charLocal
  x$chrBerlinLocal <- berlinLocal
  x$posixBerlinLocal <- as.POSIXct(berlinLocal, tz = "Europe/Berlin")
  
  x$intBerlinLocal <- as.integer(x$posixBerlinLocal)
  x$dt <- c(NA, diff(x$intBerlinLocal))
  x$chrTimezone <- format(x$posixBerlinLocal, "%Z")
  x$DST <- daylightSavingTimeInEffect(x$posixBerlinLocal)
  
  #x$numUTCOffset <- berlinLocal$utcOffset
  
  x$numUTCOffset <- utcOffset(
    x$chrBerlinLocal, berlinNormalTimeToUTC(x$chrBerlinNormal)
  )
  
  x$backToUTC <- toUTC(x$posixBerlinLocal)

  x2 <- x
  
  print(x2)
  
  invisible(list(x1, x2))
}

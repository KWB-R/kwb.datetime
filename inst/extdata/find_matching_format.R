#remotes::install_github("kwb-r/kwb.logger@dev")

# Create example timestamp sequences -------------------------------------------
if (FALSE)
{
  as_berlin_posix <- function(x) as.POSIXct(x, tz = "Europe/Berlin")
  as_berlin_normal_posix <- function(x) as.POSIXct(x, tz = "Etc/GMT-1")
  
  first_day <- "2019-01-01"
  last_day <- "2020-01-01"
  
  timestamps_berlin_1h_1year <- format(seq(
    as_berlin_posix(first_day), as_berlin_posix(last_day), 3600
  ))

  timestamps_berlin_normal_1h_1year <- format(seq(
    as_berlin_normal_posix(first_day), as_berlin_posix(last_day), 3600
  ))

  kwb.datetime::getTimestampSummary(timestamps_berlin_1h_1year)
  kwb.datetime::getTimestampSummary(timestamps_berlin_normal_1h_1year)
    
  times_berlin <- as.POSIXct(timestamps_berlin_1h_1year)
  times_berlin_normal <- as.POSIXct(timestamps_berlin_normal_1h_1year)
  
  table(diff(times_berlin))
  table(diff(times_berlin_normal))
  
  plot(kwb.datetime::getEqualStepRanges(times_berlin))
  plot(kwb.datetime::getEqualStepRanges(times_berlin_normal))

  kwb.datetime::getEqualStepRanges(times_berlin_normal)
  
  analyse_timestamps(timestamps_berlin_1h_1year)
}

# main -------------------------------------------------------------------------
if (FALSE)
{
  timestamp_list <- kwb.logger::getExampleTimestamps()
  
  writeHeadLines(timestamp_list, n = 2)

  templates <- sapply(timestamp_list, "[", 1)
  
  formats <- kwb.datetime::matchingTimeFormat(templates, method = 2)
  
  data.frame(template = templates, format = formats)
  
  analyse_timestamps(timestamps = "1.12.2019")
  
  tss <- kwb.datetime::getTimestampSummary(timestamp_list[[1]])
  
  for (i in seq_along(templates)) {
    message(templates[i])
    analyse_timestamps(timestamps = timestamp_list[[i]])
  }
  
  iso_timestamps <- kwb.utils::getAttribute(tss, "iso_timestamps")
  
  # Does the clock adjust for daylight saving time or not?
  times_1 <- kwb.datetime::textToEuropeBerlinPosix(iso_timestamps, switches = TRUE)
  times_2 <- kwb.datetime::textToEuropeBerlinPosix(iso_timestamps, switches = FALSE)
}

# analyse_timestamps -----------------------------------------------------------
analyse_timestamps <- function(timestamps, ...)
{
  tss <- kwb.datetime:::getTimestampSummary(timestamps, template_index = 1)

  print(tss)
  
  # Get timestamps in ISO format
  iso_timestamps <- kwb.utils::getAttribute(tss, "iso_timestamps")

  times <- as.POSIXct(iso_timestamps, ...)
  
  compareTimestamps(
    original = iso_timestamps, 
    backcalculated = format(times, format = "%Y-%m-%d %H:%M:%S"), 
    "Differences after converting back to ISO format"
  )
  
  compareTimestamps(
    original = timestamps, 
    backcalculated = format(times, format = tss$timestamp_format),
    "Differences after converting back to original format"
  )
}

# compareTimestamps ------------------------------------------------------------
compareTimestamps <- function(original, backcalculated, title)
{
  differs <- original != backcalculated
  kwb.datetime:::catf(paste0(title, ": %d"), sum(differs))
  
  if (any(differs)) {
    
    indices <- head(which(differs))
    
    print.data.frame(row.names = FALSE, data.frame(
      original = original[indices], 
      backcalculated = backcalculated[indices]
    ))
  }
}

# writeHeadLines ---------------------------------------------------------------
writeHeadLines <- function(x, n = 6)
{
  stopifnot(is.list(x), all(sapply(x, is.character)))
  
  invisible(lapply(x, function(xx) {
    writeLines(head(xx, n))
    cat("\n")
  }))

}

#remotes::install_github("kwb-r/kwb.logger@dev")

# Create example timestamp sequences -------------------------------------------
if (FALSE)
{
  as_berlin_posix <- function(x) as.POSIXct(x, tz = "Europe/Berlin")
  as_berlin_normal_posix <- function(x) as.POSIXct(x, tz = "GMT-1")
  
  first_day <- "2019-01-01"
  last_day <- "2020-01-01"
  
  timestamps_berlin_1h_1year <- format(seq(
    as_berlin_posix(first_day), as_berlin_posix(last_day), 3600
  ))

  timestamps_berlin_normal_1h_1year <- format(seq(
    as_berlin_normal_posix(first_day), as_berlin_posix(last_day), 3600
  ))

  kwb.datetime:::getTimestampSummary(timestamps_berlin_1h_1year)
  kwb.datetime:::getTimestampSummary(timestamps_berlin_normal_1h_1year)
    
  times_berlin <- as.POSIXct(timestamps_berlin_1h_1year)
  times_berlin_normal <- as.POSIXct(timestamps_berlin_normal_1h_1year)
  
  table(diff(times_berlin))
  table(diff(times_berlin_normal))
  
  plot(kwb.datetime:::getEqualStepRanges(times_berlin))
  plot(kwb.datetime:::getEqualStepRanges(times_berlin_normal))

  kwb.datetime:::getEqualStepRanges(times_berlin_normal)
  
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
  
  tss <- kwb.datetime:::getTimestampSummary(timestamp_list[[1]])
  
  for (i in seq_along(templates)) {
    message(templates[i])
    analyse_timestamps(timestamps = timestamp_list[[i]])
  }
  
  iso_timestamps <- kwb.utils::getAttribute(tss, "iso_timestamps")
  
  # Does the clock adjust for daylight saving time or not?
  times <- lapply(c(TRUE, FALSE), function(dst_shift) if (dst_shift) {
    kwb.datetime::berlin_local_timestamps_to_POSIXct(iso_timestamps)
  } else {
    structure(as.POSIXct(iso_timestamps, tz = "Etc/GMT-1"), tzone = "")
  })
  
  lapply(times, head)
}

# plot.equal_step_range --------------------------------------------------------
plot.equal_step_range <- function(x, format = "%d.%m.%Y %H:%M", ...)
{
  stopifnot(inherits(x, "equal_step_range"))

  times <- kwb.utils::getAttribute(x, "times")

  xlim <- kwb.utils::hsRestoreAttributes(
    c(min(x$from_time), max(x$to_time)), 
    attribs = attributes(x$from_time)
  )

  n_periods <- nrow(x)
  
  old_par <- par(mar = c(2.5, 1, 3, 13))
  on.exit(par(old_par))
  
  plot(x$from_time[1], 1, xlim = xlim, ylim = c(n_periods, 1), type = "n",
       xlab = "", ylab = "", yaxt = "n", main = "Sequences of equal time step")
  
  x_offset <- kwb.plot::cmToUserWidthAndHeight(1)$width
  
  for (i in seq_len(n_periods)) {
    xx <- times[x$from[i]:x$to[i]]
    points(xx, rep(i, length(xx)))
    text(xlim[2] + x_offset, i, xpd = TRUE, cex = 0.8, adj = 0, sprintf(
      "%s - %s\nstep = %s", 
      format(x$from_time[i], format = format), 
      format(x$to_time[i], format = format), 
      x$step[i]
    ))
  }
}

# print.timestamp_summary ------------------------------------------------------
print.timestamp_summary <- function(x, ...)
{
  catf("Number of timestamps: %d", x$number_of_timestamps)
  catf("Number of duplicated timestamps: %d", x$number_of_duplicated_timestamps)
  catf("Timestamp template: %s", x$timestamp_template)
  catf("(Determined) Timestamp format: %s", x$timestamp_format)
  catf("Number of days involved: %d", x$number_of_days_involved)
  catf("Range of days involved: %s", paste(
    x$range_of_days_involved, collapse = "--"
  ))
  catf("Number of years involved: %d", length(x$unique_years))
  catf("Years involved: %s", paste(x$unique_years, collapse = ", "))
  catf(
    "Number of 'switch' days (CET <-> CEST) involved: %d", 
    x$number_of_switch_days
  )
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
  catf(paste0(title, ": %d"), sum(differs))
  
  if (any(differs)) {
    
    indices <- head(which(differs))
    
    print.data.frame(row.names = FALSE, data.frame(
      original = original[indices], 
      backcalculated = backcalculated[indices]
    ))
  }
}

# catf -------------------------------------------------------------------------
catf <- function(x, ...)
{
  cat(sprintf(paste0(x, "\n"), ...))
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

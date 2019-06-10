# getTimestampSummary ----------------------------------------------------------
getTimestampSummary <- function(timestamps, format = NULL, template_index = NULL)
{
  if (is.null(template_index)) {
    template_index <- sample(length(timestamps), 1)
  }
  
  timestamp_template <- timestamps[template_index]
  
  if (is.null(format)) {
    timestamp_format <- matchingTimeFormat(timestamp_template)
  }
  
  # Convert timestamps to ISO format
  iso_timestamps <- reformatTimestamp(timestamps, timestamp_format)
  
  # Which days are involved?
  unique_daystrings <- sort(unique(substr(iso_timestamps, 1, 10)))
  
  # Which years are involved?
  unique_years <- sort(unique(as.integer(substr(unique_daystrings, 1, 4))))
  
  # At what days would time be switched in these years?
  switch_days <- c(date_range_CEST(unique_years))
  
  structure(
    list(
      number_of_timestamps = length(timestamps),
      number_of_duplicated_timestamps = sum(duplicated(timestamps)),
      timestamp_template = timestamp_template,
      timestamp_format = timestamp_format,
      number_of_days_involved = length(unique_daystrings),
      range_of_days_involved = range(unique_daystrings),
      unique_years = unique_years,
      number_of_switch_days = sum(switch_days %in% unique_daystrings)
    ), 
    iso_timestamps = iso_timestamps,
    unique_daystrings = unique_daystrings,
    switch_days = switch_days,
    class = "timestamp_summary"
  )
}

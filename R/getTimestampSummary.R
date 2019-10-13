# getTimestampSummary ----------------------------------------------------------

#' Summary about a Sequence of (Text) Timestamps
#' 
#' @param x vector of character representing timestamps
#' @param format format description, such as "\%Y-\%m-\%d \%H:\%M:\%S"
#' @param template_index index in \code{x} from which to select a timestamps 
#'   that is used as a template when looking for an appropriate timestamp format
#' @export
#' @examples 
#' x <- kwb.datetime::sequenceOfTimestamps("2019-10-31", "2019-11-01")
#' getTimestampSummary(x)
#' 
getTimestampSummary <- function(x, format = NULL, template_index = NULL)
{
  if (is.null(template_index)) {
    template_index <- sample(length(x), 1)
  }
  
  timestamp_template <- x[template_index]
  
  if (is.null(format)) {
    timestamp_format <- matchingTimeFormat(timestamp_template)
  }
  
  # Convert x to ISO format
  iso_timestamps <- reformatTimestamp(x, timestamp_format)
  
  # Which days are involved?
  unique_daystrings <- sort(unique(substr(iso_timestamps, 1, 10)))
  
  # Which years are involved?
  unique_years <- sort(unique(as.integer(substr(unique_daystrings, 1, 4))))
  
  # At what days would time be switched in these years?
  switch_days <- c(date_range_CEST(unique_years))
  
  structure(
    list(
      number_of_timestamps = length(x),
      number_of_duplicated_timestamps = sum(duplicated(x)),
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
    class = "timestampSummary"
  )
}

# print.timestampSummary -------------------------------------------------------

#' @export
#' @keywords internal
#' 
print.timestampSummary <- function(x, ...)
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

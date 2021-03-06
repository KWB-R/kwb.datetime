% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{isoToLocaltime}
\alias{isoToLocaltime}
\title{Text Timestamps to POSIXct}
\usage{
isoToLocaltime(timestamps, dbg = TRUE)
}
\arguments{
\item{timestamps}{vector of character timestamps of format \code{yyyy-mm-dd 
HH:MM:SS+[01|02]}, i.e. ending either in '+0100' (UTC offset in Berlin in
winter) or '+0200' (UTC offset in Berlin in summer)}

\item{dbg}{if \code{TRUE} debug messages are shown}
}
\description{
Convert text timestamps in a format according to ISO 8601 to POSIXct objects
}
\examples{
times <- isoToLocaltime(c(
  "2017-10-29 01:00:00+0200", 
  "2017-10-29 01:30:00+0200", 
  "2017-10-29 02:00:00+0200",
  "2017-10-29 02:30:00+0200",
  "2017-10-29 02:00:00+0100", 
  "2017-10-29 02:30:00+0100", 
  "2017-10-29 03:00:00+0100", 
  "2017-10-29 03:30:00+0100"
))

class(times)
plot(times, rep(1, length(times)), ylab = "", xlab = "LocalTime")

}

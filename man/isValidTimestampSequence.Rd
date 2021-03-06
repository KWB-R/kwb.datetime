% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step.R
\name{isValidTimestampSequence}
\alias{isValidTimestampSequence}
\title{Check Sequence of Timestamps for Validity}
\usage{
isValidTimestampSequence(
  timestamps,
  checks = c("sorted", "duplicates", "timestep"),
  dbg = FALSE
)
}
\arguments{
\item{timestamps}{vector of timestamps of class POSIXt}

\item{checks}{Vector of character indicating the checks to be performed.
Available checks: "sorted", "duplicates", "timestep". Default: perform all
tests}

\item{dbg}{shall debug messages be shown?}
}
\description{
Different validation checks for sequence of timestamps
}
\examples{
timestamps <- sequenceOfTimestamps("2017-11-03", "2017-11-04", 3600)
times <- as.POSIXct(timestamps)
isValidTimestampSequence(times)
isValidTimestampSequence(rev(times))
isValidTimestampSequence(timestamps = c(times[1], times))

}

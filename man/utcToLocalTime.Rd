% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{utcToLocalTime}
\alias{utcToLocalTime}
\title{utcToLocalTime}
\usage{
utcToLocalTime(x, tz)
}
\arguments{
\item{x}{Character timestamp that is interpretable by
\code{\link{as.POSIXct}}, representing a time given in UTC}

\item{tz}{string representing the timezone to which the timestamp is to be
converted}
}
\description{
utcToLocalTime
}
\keyword{internal}

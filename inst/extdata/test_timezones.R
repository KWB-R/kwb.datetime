timestamps <- kwb.datetime::sequenceOfTimestamps("2015-01-01", "2017-01-01")

length(timestamps)

kwb.datetime::date_range_CEST(2016)

i <- grep("^2016-10-30 03:00:00", timestamps)

indices_cest_cet <- (i-30):(i+30)

timestamps[indices_cest_cet]

times <- as.POSIXct(timestamps, tz = "UTC")

times_berlin <- kwb.datetime:::toTimezone(times, "Europe/Berlin")

times_berlin[indices_cest_cet - 60]

head(times_berlin)

timestamps <- substr(format(times_berlin, format = "%F %T%z"), 1, 22)

head(timestamps)

system.time(local1 <- kwb.datetime::iso_to_localtime(timestamps))
system.time(local2 <- as.POSIXct(paste0(timestamps, "00"), format = "%F %T%z"))

all(local1 == local2)

times <- as.POSIXct(rep(NA, 2))

times[1] <- as.POSIXct("2016-10-30 02:00:00", tz = "Etc/GMT-2")
times[2] <- as.POSIXct("2016-10-30 02:00:00", tz = "Etc/GMT-1")

times
structure(times, tzone = "UTC")

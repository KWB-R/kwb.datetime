# kwb.datetime 0.4.0.9000

* Added a `NEWS.md` file to track changes to the package (see 
https://style.tidyverse.org/news.html for writing a good `NEWS.md`)
* Fix bug in hasTimeFormat(): return vector of boolean also if method = 2

# kwb.datetime 0.4.0

* New vignettes: "Converting Text to Time Objects", 
  "Use Case: Importing Timestamps"
* New functions: getEqualStepRanges(), getTimestampSummary(), 
  textToEuropeBerlinPosix()
* hasTimeFormat(): add argument "method"
* iso_to_localtime(): rename to isoToLocaltime()
* matchingTimeFormat(): add arguments "method", "warn", "failureValue"; 
  allow to pass a vector of timestamps
* utc_offset_Berlin_time(): rename to utcOffsetBerlinTime()

# kwb.datetime 0.3.1

* Add vignette "Exkurs: Zeitzonen" (in German!)

# kwb.datetime 0.1.0

* package created
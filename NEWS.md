# [kwb.datetime 0.5.0](https://github.com/KWB-R/kwb.datetime/releases/tag/v0.5.0) <small>2021-07-07</small>

* Fix installation of GitHub dependencies via `Remotes` field 
* Use Github Actions for continuous integration
* Fix bug in `hasTimeFormat()`: return vector of boolean also if method = 2
* New function: `timesAroundClockChange()` (moved from R package 
[kwb.test.datetime](https://github.com/kwb-r/kwb.test.datetime))
* New function: `test_TimeConversionFunctions()` (moved from R package 
[kwb.test.datetime](https://github.com/kwb-r/kwb.test.datetime))
* Added a `NEWS.md` file to track changes to the package (see 
https://style.tidyverse.org/news.html for writing a good `NEWS.md`)

# [kwb.datetime 0.4.0](https://github.com/KWB-R/kwb.datetime/releases/tag/v0.4.0) <small>2019-10-13</small>

* New vignettes: [Converting Text to Time Objects](../articles/text_to_posixct.html), 
  "Use Case: Importing Timestamps"
* New functions: `getEqualStepRanges()`, `getTimestampSummary()`, 
  `textToEuropeBerlinPosix()`
* `hasTimeFormat()`: add argument "method"
* `iso_to_localtime()`: rename to `isoToLocaltime()`
* `matchingTimeFormat()`: add arguments "method", "warn", "failureValue"; 
  allow to pass a vector of timestamps
* `utc_offset_Berlin_time()`: rename to `utcOffsetBerlinTime()`
* Add vignette [Exkurs: Zeitzonen" (in German!)](../articles/timezones.html)

# [kwb.datetime 0.3.0](https://github.com/KWB-R/kwb.datetime/releases/tag/v0.3.0) <small>2017-11-06</small>

* First release on GitHub (as rev. 4986 on KWB's Subversion server)
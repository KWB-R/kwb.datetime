#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("isValidTimestampSequence() works", {

  kwb.datetime:::isValidTimestampSequence(timestamps = as.POSIXct("2018-06-03 23:50:00"))
   expect_error(
    kwb.datetime:::isValidTimestampSequence(timestamps = 1)
    # "POSIXt" %in% class(timestamps) is not TRUE
  )

})


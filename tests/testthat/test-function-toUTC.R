#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("toUTC() works", {

  kwb.datetime:::toUTC(x = as.POSIXct("2018-06-03 23:50:00"))
   expect_error(
    kwb.datetime:::toUTC(x = 1)
    # inherits(x, "POSIXt") is not TRUE
  )

})


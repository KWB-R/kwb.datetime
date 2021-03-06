#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("hsTsIn() works", {

  expect_error(
    kwb.datetime:::hsTsIn(... = 1)
    # unused argument (... = 1)
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = 1:2)
    # unused argument (... = 1:2)
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = "a")
    # unused argument (... = "a")
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = c("a", "b"))
    # unused argument (... = c("a", "b"))
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = TRUE)
    # unused argument (... = TRUE)
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = FALSE)
    # unused argument (... = FALSE)
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = as.POSIXct("2018-06-03 23:50:00"))
    # unused argument (... = 1528062600)
  )
   expect_error(
    kwb.datetime:::hsTsIn(... = list(key = c("a", "b"), value = 1:2))
    # unused argument (... = list(key = c("a", "b"), value = 1:2))
  )

})


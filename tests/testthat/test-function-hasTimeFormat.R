test_that("hasTimeFormat() works", {
   
   f <- kwb.datetime:::hasTimeFormat
   
   expect_identical(f(timestamps = 1), FALSE)
   expect_identical(f(timestamps = 1:2), c(FALSE, FALSE))
   expect_identical(f(timestamps = "a"), FALSE)
   expect_identical(f(timestamps = c("a", "b")), c(FALSE, FALSE))
   expect_true(f(timestamps = as.POSIXct("2018-06-03 23:50:00")))

   x <- c("2010-31-12 00:00:00", "2010-12-31 00:00:00")
   y <- c(FALSE, TRUE)
   expect_identical(f(x, method = 1L), y)
   expect_identical(f(x, method = 2L), y)
   
   fmt <- "%Y-%d-%m 00:00:00"
   y <- c(TRUE, FALSE)
   expect_identical(f(x, method = 1L, timeformat = fmt), y)
   expect_identical(f(x, method = 2L, timeformat = fmt), y)
})

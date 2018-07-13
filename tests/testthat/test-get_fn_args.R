context("Test main function")

test_that("Returns correct arguments unchanged",{
          expect_equal(get_fn_args("x<-4", return_string = TRUE),
                       "x<-4")
          expect_equal(get_fn_args("x<-4; y = 5", return_string = TRUE),
                       "x<-4; y = 5")
          expect_equal(get_fn_args("x<-4; y = 5; assign(xer, 56)", return_string = TRUE),
                       "x<-4; y = 5; assign(xer, 56)")
})

test_that("Removes bad arguments",{
  expect_equal(get_fn_args("x<-4; bad argument; another bad argument", return_string = TRUE),
               "x<-4")
  expect_equal(get_fn_args("x<-4; y = 5; 12; ; lo; sss; 987;", return_string = TRUE),
               "x<-4; y = 5")
  expect_equal(get_fn_args("x<-4; y = 5; assign(xer, 56); <>,./.", return_string = TRUE),
               "x<-4; y = 5; assign(xer, 56)")
})

test_that("Replaces commas",{
  expect_equal(get_fn_args("x<-4, z = list(1,2,3), a = matrix([]), d= 4", return_string = TRUE),
               "x<-4; z = list(1,2,3); a = matrix([]); d= 4")
  expect_equal(get_fn_args("x<-4, z = list(1,2,3), a = matrix([]), d= 4, bad, remove", return_string = TRUE),
               "x<-4; z = list(1,2,3); a = matrix([]); d= 4")
})

test_that("Argument valid_only works as intended",{
  expect_equal(get_fn_args("x<-4; bad argument; another bad argument", valid_only = FALSE, return_string = TRUE),
               "x<-4; bad argument; another bad argument")
  expect_equal(get_fn_args("x<-4; y = 5; 12; ; lo; sss; 987;", valid_only = FALSE, return_string = TRUE),
               "x<-4; y = 5; 12; ; lo; sss; 987;")
  expect_equal(get_fn_args("x<-4; y = 5; assign(xer, 56); <>,./.", valid_only = FALSE, return_string = TRUE),
               "x<-4; y = 5; assign(xer, 56); <>;./.")
  expect_equal(get_fn_args("x<-4, z = list(1,2,3), a = matrix([]), d= 4, bad, remove", valid_only = FALSE, return_string = TRUE),
               "x<-4; z = list(1,2,3); a = matrix([]); d= 4; bad; remove")
})



test_that("Removes uneccessary parts",{
  expect_equal(get_fn_args("  x<-4", return_string = TRUE),
               "x<-4")
  expect_equal(get_fn_args("{{ ((  x<-4; y = 5", return_string = TRUE),
               "x<-4; y = 5")
  expect_equal(get_fn_args("{{((  ({x<-4; y = 5; assign(xer, 56)", return_string = TRUE),
               "x<-4; y = 5; assign(xer, 56)")
})
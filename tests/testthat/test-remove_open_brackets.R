context("test remove_open_brackets function works as expected")

test_that("Removes uneccessary parts",{
  expect_equal(remove_open_brackets("  x<-4"),
               "x<-4")
  expect_equal(remove_open_brackets("{{ ((  x<-4; y = 5"),
               "x<-4; y = 5")
  expect_equal(remove_open_brackets("{{((  ({x<-4; y = 5; assign(xer, 56)"),
               "x<-4; y = 5; assign(xer, 56)")
})


test_that("Leaves correct arguments untouched",{
  expect_equal(remove_open_brackets("$%%ggt^&^&&&&^%$^"),
               "$%%ggt^&^&&&&^%$^")
  expect_equal(remove_open_brackets("-  {90000}})00"),
               "-  {90000}})00")
  expect_equal(remove_open_brackets("pepeoeoe  55 r{})))"),
               "pepeoeoe  55 r{})))")
  expect_equal(remove_open_brackets("}}(()))"),
               "}}(()))")
})
context("validate_code correctly removes bad arguments and keeps good ones")

test_that("Unassigned arguments are not kept",{
          expect_equal("", validate_code("noassignment"))
          expect_equal("", validate_code("< - a"))
          expect_equal("", validate_code(""))
          expect_equal("", validate_code("ddkddkdkdkddkjf ehjnejej fjhffjoromaPA 122343 ':<>?\\{}[]'#@~¬!£$%^&*-_"))
          })

test_that("assigned arguments are kept",{
  expect_equal("x <-4 ", validate_code("x <-4 "))
  expect_equal("x  = 4 ", validate_code("x  = 4 "))
  expect_equal("assign(x,3)", validate_code("assign(x,3)"))
  expect_equal("assign(x,3); x <- 4", validate_code("assign(x,3); x <- 4"))
  expect_equal("y = 5; x <- 4", validate_code("y = 5; x <- 4"))
  expect_equal("y = 5; assign(x,3)", validate_code("y = 5; assign(x,3)"))
  expect_equal("x <-4 ",
               validate_code("x <-4 ; bad argument; another bad argument; no assign here"))
  expect_equal("x  = 4",
               validate_code("x  = 4; bad one; 1233; dde  "))
  expect_equal("assign(x,3)",
               validate_code("assign(x,3); rrto;   ; ff"))
  expect_equal("assign(x,3); x <- 4",
               validate_code("assign(x,3); x <- 4; l ; or; 1r9"))
  expect_equal("y = 5; x <- 4",
               validate_code("y = 5; x <- 4; over;; $$55; %%"))
  expect_equal("y = 5; assign(x,3)",
               validate_code("y = 5; assign(x,3); ; ; ; 88; uoi; ppop"))
})

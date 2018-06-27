context("Test str_iterate correctly finds commas in strings")

text_length10 <- "1234567890"
text_length5 <- "12345"
text_length3 <- "123"
text_length2 <- "12"

test_that(
  "Commas are found in straight forward strings",{
  expect_equal(c(4, 10, 12, 23),
               str_iterate(
                 paste(text_length3, text_length5, "1", text_length10, "1", sep = ",")
                 )
               )
  expect_equal(c(4), str_iterate(paste(text_length3, text_length3, sep = ",")))
  expect_equal(c(2), str_iterate("a,f"))
  expect_equal(c(2, 103), str_iterate(paste0("r,", paste(rep(text_length10, 10), collapse = ""), ",o")))
}
)


test_that(
  "Correct output when no simple commas are present",{
    expect_null(str_iterate("nocommas"))
    expect_null(str_iterate(""))
    expect_null(str_iterate("nocommas_here**£$!^&'@_-`¬/?:;}{()(())}}"))
  }
)

test_that(
  "Correct output when no commas are present",{
    expect_null(str_iterate("nocommas (some in here,, ,,d,,`` {and, here, ' and here' })"))
    expect_null(str_iterate("commas \",\" but also\' \\\""))
    expect_null(str_iterate("{(,),\",\", `,`,',',,{,{,{,{,}}}}}"))
  }
)
test_that(
  "Correct output for tricky inputs",{
    expect_equal(
      c(4, 10, 42, 103),
      str_iterate("x=3, z =5, z = \"hello there, i am a cat!\", a = list(\"vat, dfl\", x = function(x,y,z){a <- list(1,2,3)}), d = 5")
      )
    expect_equal(
      c(4, 58),
      str_iterate("x=3, z = \"this is going to cause \\\", some real problems \", a = \"cakes\" ")
      )
    expect_equal(
      c(4, 41, 68),
      str_iterate("x=4, z = \" here is a sneaky ' person , \", y = ' now see, the issue', d =5")
      )
  }
)



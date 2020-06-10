

test_that("coef works", {
  fit<-blblm(mpg~wt,mtcars,m=3,B=1000)
  expect_equal(length(coef(fit)),2)
  expect_equal(mode(coef(fit)), "numeric")
})


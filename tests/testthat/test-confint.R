test_that("confint works", {
  fit<-blblm(mpg~wt,mtcars,m=3,B=1000)

  expect_equal(length(confint(fit)),2)
  expect_equal(mode(confint(fit)), "numeric")
})



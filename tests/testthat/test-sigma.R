test_that("sigma works",{
  fit<-blblm(mpg~wt,mtcars,m=3,B=1000)

  expect_equal(length(sigma(fit)),1)
  expect_equal(mode(sigma(fit)), "numeric")

})

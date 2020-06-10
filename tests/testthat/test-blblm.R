test_that("blblm works", {
  fit<-blblm(mpg~wt,mtcars,m=3,B=1000)
  expect_equal(length(fit$estimates), 3)
  expect_equal(length(fit$estimates[[1]][[1]]), 2)
})

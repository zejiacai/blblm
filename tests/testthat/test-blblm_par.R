

library(parallel)

test_that("blblm_par works", {
  cl<- makeCluster(2)
  model= blblm_par(mpg~wt,mtcars,m=3,B=1000,cl=cl)

  expect_equal(length(model$estimates), 3)
  expect_equal(length(model$estimates[[1]][[1]]), 2)
  stopCluster(cl)
})

#' @import purrr
#' @import stats
#' @import parallel
#' @import readr
#' @importFrom Rcpp sourceCpp
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))




#' Title Blblm without parallelization
#'
#' @param formula formula for regression
#' @param data dataset or a list of files
#' @param m number of subset or number of files
#' @param B number of boostrap samples
#'
#' @return fitted lm
#' @export
#'
#' @examples
#' \dontrun{
#' blblm(mpg~wt,mtcars, 3,1000)
#'}
blblm <- function(formula, data, m = 3, B = 5000) {
  if (is.data.frame(data)){
    data_list <- split_data(data, m)
    }
  else if (is.data.frame(data)==FALSE) {
    data_list<-data%>%
      map(function (fname){
       as.data.frame(readr::read_csv(fname, col_types = cols()))
      })
    }

  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}



#' Blblm with parallelization
#'
#' @param formula formula for regression
#' @param data dataset
#' @param m number of subset
#' @param B number of boostrap samples
#' @param cl cluster created
#'
#' @return fitted lm
#' @export
#'
#' @examples
#' \dontrun{
#' cl<- makeCluster(4)
#' invisible(clusterEvalQ(cl, {library(tidyverse)
#' NULL}))
#' blblm_par(mpg~wt,mtcars, 3,1000 , cl)
#' stopCluster(cl)}
blblm_par<- function(formula, data, m = 3, B = 5000, cl=cl){

    data_list <- split_data(data, m)
    estimates <- parallel::parLapply(
      cl,
      data_list,
      fun=lm_each_subsample,
      formula = formula, n = nrow(data), B = B)

  results <- list(estimates = estimates, formula = formula)
  class(results) <- "blblm"
  invisible(results)
}




#' split data into m parts of approximated equal sizes
#'
#' @param data dataset of interest
#' @param m number of subset
split_data <- function(data, m) {
  idx <- sampleintC(data,m)
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula formula for regression
#' @param data dataset of interest
#' @param n number of rows in total
#' @param B number of bootstrap samples
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula formula for regression
#' @param data dataset of interest
#' @param n number of rows in total
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
#'
#' @param formula regression formula
#' @param data dataset of interest
#' @param freqs weights
lm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  fit <- lm(formula, data, weights = freqs)
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#'
#' @param fit fitted blblm model
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#'
#' @param fit fitted blblm model
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' Print the Blblm model
#'
#' @param x blblm model
#'
#' @param ... other parameters
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' Calculate sigma
#'
#' @param object blblm model
#'
#' @param confidence give a confidene interval or not
#' @param level confidence level
#' @param ... other parameters
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' Calculate coefficient
#'
#' @param object blblm model
#'
#' @param ... other parameters
#'
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}






#' Calculate the confidence interval
#'
#' @param object blblm model
#'
#' @param parm parameter wanted
#' @param level confidence  level
#' @param ... other parameters
#'
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' Prediction using the blblm model
#'
#' @param object blblm model
#'
#' @param new_data data used to predict
#' @param confidence give a confidene interval or not
#' @param level confidence level
#' @param ... other parameters
#'
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}



mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}

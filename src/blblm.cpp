#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector sampleintC(DataFrame df, int m) {

  int n= df.nrow();

  IntegerVector a= seq(1,m);

  IntegerVector indx=sample(a, n, true);

  return indx;

}



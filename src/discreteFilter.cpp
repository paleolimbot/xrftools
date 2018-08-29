#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector discreteFilter(NumericVector x, NumericVector f, bool scale, bool tails) {
  int n = x.length();
  NumericVector out(n);

  int fLength = f.length();
  int fMid = fLength / 2L;
  int filterStart;
  int filterEnd;
  int vectorStart;
  int vectorEnd;
  double filterValue;

  for(int i=0; i<n; i++) {
    filterStart = max(IntegerVector::create(0L, fMid - i));
    filterEnd = min(IntegerVector::create(fLength - 1L, n - 1L - i + fMid));
    vectorStart = max(IntegerVector::create(0, i - fMid));
    vectorEnd = min(IntegerVector::create(n - 1, i + fMid));

    NumericVector fLocal = f[seq(filterStart, filterEnd)];
    NumericVector xLocal = x[seq(vectorStart, vectorEnd)];
    filterValue = sum(fLocal * xLocal);
    if(scale) {
      filterValue = filterValue / sum(fLocal);
    }

    if(!tails && (fLocal.length() != fLength)) {
      out[i] = NA_REAL;
    } else {
      out[i] = filterValue;
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector discreteFilterIterative(NumericVector x, NumericVector f, int iterations, double epsilon) {
  int n = x.length();
  NumericVector out = x;
  NumericVector outNext(out.length());
  double meanResidual;

  for(int i=0; i<iterations; i++) {
    checkUserInterrupt();

    outNext = discreteFilter(out, f, true, true);
    meanResidual = sum(outNext - out) / n;
    if((epsilon > 0) && (meanResidual < epsilon)) {
      out = outNext;
      break;
    } else {
      out = outNext;
    }
  }

  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
v <- cumsum(rnorm(100))
# should have NAs on the end
v1 <- discreteFilter(v, c(1, 2, 3, 2, 1), TRUE, tails = FALSE)
stopifnot(sum(is.na(v1)) == 4)
# no NAs on the end
v2 <- discreteFilter(v, c(1, 2, 3, 2, 1), TRUE, tails = TRUE)
stopifnot(sum(is.na(v2)) == 0)

plot(v, type = "l", col = "black")
lines(v2, type = "l", col = "blue")

n_iters <- 10
for(iters in 1:n_iters) {
  vx <- discreteFilterIterative(v, c(1, 2, 3, 2, 1), iterations = iters, epsilon = -1)
  lines(vx, col = rgb(red = 1, green = 0, blue = 0, alpha = 1 - (iters / n_iters)))
}
*/

#include <Rcpp.h>
using namespace Rcpp;

//' Group Sorted Vector by Range
//'
//' Divide a sorted numeric vector into groups such that
//' \code{max(x[group]) - min(x[group]) <= max_range}.
//'
//' @param x A sorted numeric vector, Date vector, or POSIXt vector.
//' @param max_range A non-negative numeric threshold for the within-group range.
//' @return An IntegerVector of group IDs (1-indexed).
//' @export
// [[Rcpp::export]]
IntegerVector group_by_range_cpp(NumericVector x, double max_range) {
  int n = x.size();
  if (n == 0) {
    return IntegerVector(0);
  }

  IntegerVector group(n);
  int current_group = 1;
  double current_min = x[0];

  for (int i = 0; i < n; i++) {
    if (x[i] - current_min > max_range) {
      current_group++;
      current_min = x[i];
    }
    group[i] = current_group;
  }

  return group;
}

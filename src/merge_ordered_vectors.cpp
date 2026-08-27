#include <Rcpp.h>
using namespace Rcpp;

//' Vote-based Ordered Vector Merge (C++ core)
//'
//' Reproduces the exact swap-order semantics of
//' \code{clinpubr::merge_ordered_vectors()}: for each pair \code{(x[i], x[j])},
//' count in how many vectors \code{x[i]} precedes/follows \code{x[j]} (by first
//' occurrence); swap when the element precedes in fewer vectors. Swapping
//' updates \code{x[i]} and comparison continues, matching the original
//' bubble-style sort exactly.
//'
//' @param ids0 IntegerVector of 0-based element IDs (in initial order).
//' @param pos IntegerMatrix of shape \code{[n x K]}, column-major, where
//'   \code{pos[e, k]} is the 1-based first-occurrence position of element \code{e}
//'   in vector \code{k}, and \code{0} means not present.
//' @return An IntegerVector of 0-based IDs in merged order.
// [[Rcpp::export]]
IntegerVector mov_sort_cpp(IntegerVector ids0, IntegerMatrix pos) {
  const int n = ids0.size();
  const int K = pos.ncol();
  if (n <= 1 || K == 0) {
    return clone(ids0);
  }
  IntegerVector res = clone(ids0);
  int* x = res.begin();
  const int* base = pos.begin();
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      const int a = x[i], b = x[j];
      int before = 0, after = 0;
      for (int k = 0; k < K; ++k) {
        const int pa = base[a + k * n], pb = base[b + k * n];
        if (pa != 0 && pb != 0) {
          if (pa < pb) {
            ++before;
          } else if (pa > pb) {
            ++after;
          }
        }
      }
      if (after > before) {
        x[i] = b;
        x[j] = a;
      }
    }
  }
  return res;
}
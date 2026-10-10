// Simulation of responses from the item kernel (src/items.h).

#include <Rcpp.h>
#include "items.h"
using namespace Rcpp;

// One response per person and item: the category probabilities from
// cat_probs(), one uniform draw, inversion over the categories 0 .. K - 1.
// Persons with NA theta get an NA row and use no draws. Returns persons x items.
// [[Rcpp::export]]
IntegerMatrix pp_sim_cpp(List bank, NumericVector theta) {
  const ItemBank b = make_bank(bank);
  const int N = theta.size();
  IntegerMatrix out(N, b.n);
  std::vector<double> p(b.kmax);
  for (int i = 0; i < N; ++i) {
    if (ISNAN(theta[i])) {
      for (int j = 0; j < b.n; ++j) out(i, j) = NA_INTEGER;
      continue;
    }
    for (int j = 0; j < b.n; ++j) {
      cat_probs(b, j, theta[i], p.data());
      const double u = R::unif_rand();
      const int K = b.ncat[j];
      int x = K - 1;              // also if u falls into the rounding gap above the sum
      double cum = 0.0;
      for (int k = 0; k < K - 1; ++k) {
        cum += p[k];
        if (u < cum) { x = k; break; }
      }
      out(i, j) = x;
    }
  }
  return out;
}

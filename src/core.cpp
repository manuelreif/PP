// Internal hooks into the item kernel (src/items.h). They are registered for
// the tests (PP:::pp_item_eval_cpp, ...) and bench/kernel.R, but not exported
// in NAMESPACE.

#include <Rcpp.h>
#include "items.h"
using namespace Rcpp;

// ll, score, info and J of item j at (theta[i], X[i, j]) as four N x J
// matrices; NA responses give NA
// [[Rcpp::export]]
List pp_item_eval_cpp(List bank, NumericVector theta, IntegerMatrix X) {
  const ItemBank b = make_bank(bank);
  const int N = theta.size();
  if (X.nrow() != N || X.ncol() != b.n) stop("X must be length(theta) x n_items");
  NumericMatrix ll(N, b.n), score(N, b.n), info(N, b.n), J(N, b.n);
  std::vector<double> buf(b.kmax);
  for (int j = 0; j < b.n; ++j) {
    for (int i = 0; i < N; ++i) {
      const int x = X(i, j);
      if (x == NA_INTEGER || x < 0 || x >= b.ncat[j]) {
        ll(i, j) = score(i, j) = info(i, j) = J(i, j) = NA_REAL;
        continue;
      }
      const ItemEval e = eval_item(b, j, x, theta[i], buf.data());
      ll(i, j) = e.ll; score(i, j) = e.score; info(i, j) = e.info; J(i, j) = e.J;
    }
  }
  return List::create(_["ll"] = ll, _["score"] = score, _["info"] = info, _["J"] = J);
}

// Category probabilities: an N x sum(ncat) matrix, the categories of item 1
// first, then those of item 2, ...
// [[Rcpp::export]]
NumericMatrix pp_catprob_cpp(List bank, NumericVector theta) {
  const ItemBank b = make_bank(bank);
  const int N = theta.size();
  int ntot = 0;
  for (int j = 0; j < b.n; ++j) ntot += b.ncat[j];
  NumericMatrix out(N, ntot);
  std::vector<double> p(b.kmax);
  for (int i = 0; i < N; ++i) {
    int col = 0;
    for (int j = 0; j < b.n; ++j) {
      cat_probs(b, j, theta[i], p.data());
      for (int k = 0; k < b.ncat[j]; ++k) out(i, col++) = p[k];
    }
  }
  return out;
}

// Microbenchmark: evaluates all (person, item) pairs reps times and returns
// the sum of the scores, so the compiler cannot drop the loop. The inner loop
// runs over persons, so X is read in memory order.
// [[Rcpp::export]]
double pp_kernel_time_cpp(List bank, NumericVector theta, IntegerMatrix X, int reps) {
  const ItemBank b = make_bank(bank);
  const int N = theta.size();
  if (X.nrow() != N || X.ncol() != b.n) stop("X must be length(theta) x n_items");
  std::vector<double> buf(b.kmax);
  const int* x = INTEGER(X);
  const double* th = REAL(theta);
  double total = 0.0;
  for (int r = 0; r < reps; ++r) {
    for (int j = 0; j < b.n; ++j) {
      const int* xj = x + static_cast<size_t>(j) * N;
      for (int i = 0; i < N; ++i) {
        total += eval_item(b, j, xj[i], th[i], buf.data()).score;
      }
    }
  }
  return total;
}

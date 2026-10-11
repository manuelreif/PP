// Person fit moments and simulation of responses from the item kernel
// (src/items.h).

#include <Rcpp.h>
#include "items.h"
using namespace Rcpp;

// P'(theta) of category 1 of a 4PL item, a s p q (category 0: -P'), with p
// and q as in cat_probs()
static double dprob_4pl(const ItemBank& b, int j, double theta) {
  double z = b.a[j] * (theta - b.thr[b.off[j]]);
  z = std::min(700.0, std::max(-700.0, z));
  const double e  = std::exp(-std::fabs(z));
  const double hi = 1.0 / (1.0 + e), lo = e * hi;
  return b.a[j] * (b.d[j] - b.c[j]) * hi * lo;
}

// The item quantities person fit needs, at (theta[i], X[i, j]), as N x J
// matrices: E, V and C (mean, variance and 4th central moment of the item
// score), ll = log P(x), Ell = sum_k P_k log P_k, Vll = Var(log P),
// Cs = sum_k P'_k log P_k (the covariance of log P with the score), info and
// J (Warm). Not answered cells and persons with a non-finite theta are NA.
// [[Rcpp::export]]
List pp_moments_cpp(List bank, IntegerMatrix X, NumericVector theta) {
  const ItemBank b = make_bank(bank);
  const int N = X.nrow(), J = X.ncol();
  if (J != b.n || theta.size() != N) stop("X must be length(theta) x n_items");
  NumericMatrix E(N, J), V(N, J), C(N, J), ll(N, J), Ell(N, J), Vll(N, J), Cs(N, J),
    info(N, J), Jw(N, J);
  std::vector<double> lp(b.kmax), p(b.kmax), buf(b.kmax);
  for (int j = 0; j < J; ++j) {
    const int K = b.ncat[j];
    for (int i = 0; i < N; ++i) {
      const int x = X(i, j);
      const double t = theta[i];
      if (x == NA_INTEGER || x < 0 || x >= K || !R_finite(t)) {
        E(i, j) = V(i, j) = C(i, j) = ll(i, j) = Ell(i, j) = Vll(i, j) = Cs(i, j) =
          info(i, j) = Jw(i, j) = NA_REAL;
        continue;
      }
      log_cat_probs(b, j, t, lp.data());
      double e = 0.0, el = 0.0;
      for (int k = 0; k < K; ++k) {
        p[k] = std::exp(lp[k]);
        e += k * p[k];
        el += p[k] * lp[k];
      }
      double v = 0.0, c4 = 0.0, vl = 0.0;
      for (int k = 0; k < K; ++k) {
        const double dk = k - e, dl = lp[k] - el;
        v += dk * dk * p[k];
        c4 += dk * dk * dk * dk * p[k];
        vl += dl * dl * p[k];
      }
      double cs = 0.0;
      if (b.model[j] == 0) {
        cs = dprob_4pl(b, j, t) * (lp[1] - lp[0]);
      } else {
        // GPCM: P'_k = a P_k (k - E)
        for (int k = 0; k < K; ++k) cs += b.a[j] * p[k] * (k - e) * lp[k];
      }
      const ItemEval ev = eval_item(b, j, x, t, buf.data());
      E(i, j) = e; V(i, j) = v; C(i, j) = c4;
      ll(i, j) = lp[x]; Ell(i, j) = el; Vll(i, j) = vl; Cs(i, j) = cs;
      info(i, j) = ev.info; Jw(i, j) = ev.J;
    }
  }
  return List::create(_["E"] = E, _["V"] = V, _["C"] = C, _["ll"] = ll, _["Ell"] = Ell,
                      _["Vll"] = Vll, _["Cs"] = Cs, _["info"] = info, _["J"] = Jw);
}

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

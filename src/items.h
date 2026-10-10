// The item kernel: one numerically stable implementation of every item-level
// quantity for the 4PL and the GPCM.
//
// make_bank() converts the R item bank (built by make_bank_r() in R/prepare.R)
// once per call; it is the only function here that touches R objects. All
// kernel functions work on plain doubles, allocate nothing and call no R API,
// so they can later run inside OpenMP threads.

#ifndef PP_ITEMS_H
#define PP_ITEMS_H

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>

struct ItemBank {
  int n;                          // number of items
  int kmax;                       // largest number of categories
  std::vector<int>    model;      // 0 = 4PL, 1 = GPCM
  std::vector<int>    ncat;       // number of categories (4PL: 2)
  std::vector<int>    off;        // offset of the item's thresholds in thr
  std::vector<double> a, c, d;    // slope, lower and upper asymptote (GPCM: c = 0, d = 1)
  std::vector<double> thr;        // 4PL: b; GPCM: delta_1 .. delta_{K-1}
  std::vector<double> hub_loc;    // location of the Huber residual (4PL: b, GPCM: mean(delta))
};

// log-likelihood, score d/dtheta log P(x), Fisher information and Warm's J
// (sum_k P'_k P''_k / P_k) of one item
struct ItemEval { double ll, score, info, J; };

inline ItemBank make_bank(const Rcpp::List& b) {
  ItemBank bank;
  bank.n       = Rcpp::as<int>(b["n"]);
  bank.model   = Rcpp::as<std::vector<int> >(b["model"]);
  bank.ncat    = Rcpp::as<std::vector<int> >(b["ncat"]);
  bank.off     = Rcpp::as<std::vector<int> >(b["off"]);
  bank.a       = Rcpp::as<std::vector<double> >(b["a"]);
  bank.c       = Rcpp::as<std::vector<double> >(b["c"]);
  bank.d       = Rcpp::as<std::vector<double> >(b["d"]);
  bank.thr     = Rcpp::as<std::vector<double> >(b["thr"]);
  bank.hub_loc = Rcpp::as<std::vector<double> >(b["hub_loc"]);

  const size_t n = bank.n;
  if (bank.model.size() != n || bank.ncat.size() != n || bank.off.size() != n ||
      bank.a.size() != n || bank.c.size() != n || bank.d.size() != n || bank.hub_loc.size() != n) {
    Rcpp::stop("item bank: the item fields must have length n");
  }
  size_t nthr = 0;
  bank.kmax = 2;
  for (size_t j = 0; j < n; ++j) {
    if (bank.ncat[j] < 2 || (bank.model[j] == 0 && bank.ncat[j] != 2) ||
        bank.off[j] != static_cast<int>(nthr)) {
      Rcpp::stop("item bank: invalid ncat/off for item %d", static_cast<int>(j) + 1);
    }
    nthr += bank.ncat[j] - 1;
    bank.kmax = std::max(bank.kmax, bank.ncat[j]);
  }
  if (bank.thr.size() != nthr) Rcpp::stop("item bank: thr must have sum(ncat - 1) entries");
  return bank;
}

// 4PL: P = c + s p, Q = (1 - d) + s q with p = plogis(z), q = plogis(-z), s = d - c.
// Q is never computed as 1 - P. With P' = a s p q and P'' = a (q - p) P':
//   score = (x - P) P' / (P Q), info = P'^2 / (P Q), J = P' P'' / (P Q).
// LL = false skips the log-likelihood (ll = 0); the person estimator only
// needs score, info and J. (eval_item passes LL on to the 4PL only.)
template <bool LL = true>
inline ItemEval eval_4pl(double a, double b, double c, double d, int x, double theta) {
  const double z0  = a * (theta - b);
  const double z   = std::min(700.0, std::max(-700.0, z0));  // p and q stay > 0
  const double e   = std::exp(-std::fabs(z));
  const double inv = 1.0 / (1.0 + e);
  const double lo  = e * inv, hi = inv;                      // plogis(-|z|), plogis(|z|)
  const double p   = z >= 0 ? hi : lo;
  const double q   = z >= 0 ? lo : hi;
  const double s   = d - c;
  const double P   = c + s * p;
  const double Q   = (1.0 - d) + s * q;
  const double rp  = p / P, rq = q / Q;                      // both in (0, 1/s]
  ItemEval r;
  r.info  = a * a * s * s * p * q * rp * rq;
  r.score = x ? a * s * q * rp : -a * s * p * rq;
  r.J     = a * (q - p) * r.info;
  r.ll    = 0.0;
  if (!LL) return r;
  // Without an asymptote, log P = log s + log p with log p = min(z0, 0) - log1p(e)
  // (log q likewise), from the unclamped z0, so ll stays exact in the tails
  const double ls = s == 1.0 ? 0.0 : std::log(s);
  if (x) r.ll = c == 0.0 ? ls + std::min(z0, 0.0) - std::log1p(e)  : std::log(P);
  else   r.ll = d == 1.0 ? ls + std::min(-z0, 0.0) - std::log1p(e) : std::log(Q);
  return r;
}

// GPCM in O(K): eta_0 = 0, eta_k = eta_{k-1} + a (theta - delta_k), category
// probabilities by log-sum-exp. score = a (x - E), info = a^2 Var, J = a^3 mu_3
// (moments of the item score). buf needs room for K doubles. ll is always
// computed: next to the K exp() calls its one log() costs little, and a
// variant without it measured slower.
inline ItemEval eval_gpcm(double a, const double* delta, int K, int x, double theta, double* buf) {
  double eta = 0.0, m = 0.0;
  buf[0] = 0.0;
  for (int k = 1; k < K; ++k) {
    eta += a * (theta - delta[k - 1]);
    buf[k] = eta;
    m = std::max(m, eta);
  }
  const double eta_x = buf[x];
  double sum = 0.0;
  for (int k = 0; k < K; ++k) { buf[k] = std::exp(buf[k] - m); sum += buf[k]; }
  double E = 0.0;
  for (int k = 0; k < K; ++k) { buf[k] /= sum; E += k * buf[k]; }
  double m2 = 0.0, m3 = 0.0;
  for (int k = 0; k < K; ++k) {
    const double dk = k - E;
    m2 += dk * dk * buf[k];
    m3 += dk * dk * dk * buf[k];
  }
  ItemEval r;
  r.ll    = eta_x - m - std::log(sum);
  r.score = a * (x - E);
  r.info  = a * a * m2;
  r.J     = a * a * a * m3;
  return r;
}

// Item j of the bank at (x, theta); buf needs room for bank.kmax doubles
template <bool LL = true>
inline ItemEval eval_item(const ItemBank& bank, int j, int x, double theta, double* buf) {
  const double* thr = &bank.thr[bank.off[j]];
  if (bank.model[j] == 0) return eval_4pl<LL>(bank.a[j], thr[0], bank.c[j], bank.d[j], x, theta);
  return eval_gpcm(bank.a[j], thr, bank.ncat[j], x, theta, buf);
}

// Huber weight of the residual r = a (theta - hub_loc): 1 for |r| <= H, else H / |r|
inline double huber_w(double r, double H) {
  const double ar = std::fabs(r);
  return ar <= H ? 1.0 : H / ar;
}

inline double huber_item(const ItemBank& bank, int j, double theta, double H) {
  return huber_w(bank.a[j] * (theta - bank.hub_loc[j]), H);
}

// All ncat[j] category probabilities of item j at theta, written to p
inline void cat_probs(const ItemBank& bank, int j, double theta, double* p) {
  const double a = bank.a[j];
  const double* thr = &bank.thr[bank.off[j]];
  if (bank.model[j] == 0) {
    double z = a * (theta - thr[0]);
    z = std::min(700.0, std::max(-700.0, z));
    const double e  = std::exp(-std::fabs(z));
    const double hi = 1.0 / (1.0 + e), lo = e * hi;
    const double s  = bank.d[j] - bank.c[j];
    p[0] = (1.0 - bank.d[j]) + s * (z >= 0 ? lo : hi);
    p[1] = bank.c[j] + s * (z >= 0 ? hi : lo);
    return;
  }
  const int K = bank.ncat[j];
  double eta = 0.0, m = 0.0;
  p[0] = 0.0;
  for (int k = 1; k < K; ++k) {
    eta += a * (theta - thr[k - 1]);
    p[k] = eta;
    m = std::max(m, eta);
  }
  double sum = 0.0;
  for (int k = 0; k < K; ++k) { p[k] = std::exp(p[k] - m); sum += p[k]; }
  for (int k = 0; k < K; ++k) p[k] /= sum;
}

#endif

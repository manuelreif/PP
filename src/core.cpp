// The person estimator pp_estimate_cpp() and internal hooks into the item
// kernel (src/items.h). The hooks are registered for the tests
// (PP:::pp_item_eval_cpp, ...) and bench/kernel.R, but not exported in
// NAMESPACE.

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

template <bool LL>
double kernel_time(List bank, NumericVector theta, IntegerMatrix X, int reps) {
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
        total += eval_item<LL>(b, j, xj[i], th[i], buf.data()).score;
      }
    }
  }
  return total;
}

// Huber weights of all items at each theta: an N x n_items matrix
// [[Rcpp::export]]
NumericMatrix pp_huber_cpp(List bank, NumericVector theta, double H) {
  const ItemBank b = make_bank(bank);
  const int N = theta.size();
  NumericMatrix out(N, b.n);
  for (int j = 0; j < b.n; ++j) {
    for (int i = 0; i < N; ++i) out(i, j) = huber_item(b, j, theta[i], H);
  }
  return out;
}

// Microbenchmark: evaluates all (person, item) pairs reps times and returns
// the sum of the scores, so the compiler cannot drop the loop. The inner loop
// runs over persons, so X is read in memory order. ll = false uses the kernel
// without the log-likelihood, as the person estimator does.
// [[Rcpp::export]]
double pp_kernel_time_cpp(List bank, NumericVector theta, IntegerMatrix X, int reps,
                          bool ll = true) {
  if (!ll) return kernel_time<false>(bank, theta, X, reps);
  return kernel_time<true>(bank, theta, X, reps);
}

// --- person estimator ------------------------------------------------------

// Status codes, 1-based so R can index the levels
// c("ok", "extreme", "boundary", "no_response", "not_converged") with them
enum Status { ST_OK = 1, ST_EXTREME, ST_BOUNDARY, ST_NO_RESPONSE, ST_NOT_CONVERGED };
enum Method { M_MLE = 0, M_WLE, M_MAP, M_ROBUST };

// One person: the answered items, the method and the prior. eval() sums the
// item quantities at theta and returns the estimating equation f and its
// denominator D (table in step 4 of the guide); se() gives the standard error.
struct Person {
  const ItemBank& b;
  const int* x;              // responses of the person, NA_INTEGER = not answered
  const int* items;          // answered items
  int nobs;
  int method;
  double mu, s2, H;
  double* buf;

  struct Sums { double S, I, Iw, Iw2, J; };

  Sums sums(double theta) const {
    Sums s = {0.0, 0.0, 0.0, 0.0, 0.0};
    for (int k = 0; k < nobs; ++k) {
      const int j = items[k];
      const ItemEval e = eval_item<false>(b, j, x[j], theta, buf);
      if (method == M_ROBUST) {
        const double w = huber_item(b, j, theta, H);
        s.S += w * e.score; s.Iw += w * e.info; s.Iw2 += w * w * e.info;
      } else {
        s.S += e.score;
      }
      s.I += e.info;
      s.J += e.J;
    }
    return s;
  }

  void eval(double theta, double& f, double& D) const {
    const Sums s = sums(theta);
    switch (method) {
      case M_MLE:    f = s.S;                       D = s.I;              break;
      case M_WLE:    f = s.S + s.J / (2.0 * s.I);   D = s.I;              break;
      case M_MAP:    f = s.S - (theta - mu) / s2;   D = s.I + 1.0 / s2;   break;
      default:       f = s.S;                       D = s.Iw;             break;
    }
  }

  double se(double theta) const {
    const Sums s = sums(theta);
    switch (method) {
      case M_MAP:    return 1.0 / std::sqrt(s.I + 1.0 / s2);
      case M_ROBUST: return std::sqrt(s.Iw2) / s.Iw;   // sandwich
      default:       return 1.0 / std::sqrt(s.I);
    }
  }
};

// Safeguarded Newton (Fisher scoring) on the bracket [lo, hi] with f(lo) > 0 >
// f(hi), as in rtsafe: a scoring step is taken only if it stays inside the
// bracket and is at most half as long as the step before last; otherwise the
// bracket is bisected. Every iteration shrinks the bracket, so the iteration
// cannot cycle (A8). Returns the number of iterations; theta holds the root.
static int safe_newton(const Person& p, double& theta, double lo, double hi,
                       double exac, int maxsteps, bool& converged) {
  theta = std::min(hi, std::max(lo, theta));
  double dx = hi - lo, dx_old = dx;
  converged = false;
  int it = 0;
  while (it < maxsteps) {
    ++it;
    double f, D;
    p.eval(theta, f, D);
    if (f == 0.0) { converged = true; break; }
    if (f > 0.0) lo = theta; else hi = theta;
    const double step = f / D;
    double next = theta + step;
    // A scoring step below exac ends the iteration. It has to be caught before
    // the bracket test: at the root theta + step can round to theta, which
    // sits on the bracket edge and would trigger a bisection.
    if (std::fabs(step) < exac) {
      theta = std::min(hi, std::max(lo, next));
      converged = true;
      break;
    }
    // dx_old is the step before last when the check runs
    const bool bisect = !(next > lo && next < hi) || !(std::fabs(step) <= std::fabs(dx_old) / 2.0);
    dx_old = dx;
    if (bisect) {
      dx = (hi - lo) / 2.0;
      next = lo + dx;
    } else {
      dx = step;
    }
    const double change = std::fabs(next - theta);
    theta = next;
    if (change < exac) { converged = true; break; }
  }
  return it;
}

// Person parameters for method "mle", "wle", "map" or "robust". X holds the
// responses (NA = not answered), mu/sigma2/start have one entry per person
// (start NA = logit of the relative score), range = c(lo, hi). skip_item > 0
// leaves that item (1-based) out, as in a jackknife subsample.
// Returns theta, se, nsteps and the status code of each person.
// [[Rcpp::export]]
List pp_estimate_cpp(List bank, IntegerMatrix X, std::string method, NumericVector mu,
                     NumericVector sigma2, double H, NumericVector start, NumericVector range,
                     double exac, int maxsteps, int skip_item = 0) {
  const ItemBank b = make_bank(bank);
  const int N = X.nrow(), J = X.ncol();
  if (J != b.n) stop("X must have one column per item");
  if (mu.size() != N || sigma2.size() != N || start.size() != N) {
    stop("mu, sigma2 and start must have one entry per person");
  }
  if (range.size() != 2 || !(range[0] < range[1])) stop("range must be c(lo, hi) with lo < hi");
  int m;
  if (method == "mle") m = M_MLE;
  else if (method == "wle") m = M_WLE;
  else if (method == "map") m = M_MAP;
  else if (method == "robust") m = M_ROBUST;
  else stop("unknown method '%s'", method);
  const int skip = skip_item - 1;

  // responses person by person (row-major copy), checked once
  std::vector<int> xr(static_cast<size_t>(N) * J);
  for (int j = 0; j < J; ++j) {
    for (int i = 0; i < N; ++i) {
      const int v = X(i, j);
      if (v != NA_INTEGER && (v < 0 || v >= b.ncat[j])) {
        stop("response %d of person %d to item %d is not a category of the item", v, i + 1, j + 1);
      }
      xr[static_cast<size_t>(i) * J + j] = (j == skip) ? NA_INTEGER : v;
    }
  }
  const double* pmu = REAL(mu);
  const double* ps2 = REAL(sigma2);
  const double* pst = REAL(start);
  const double lo0 = range[0], hi0 = range[1];

  NumericVector theta_out(N), se_out(N);
  IntegerVector nsteps_out(N), status_out(N);
  double* th = REAL(theta_out);
  double* se = REAL(se_out);
  int* ns = INTEGER(nsteps_out);
  int* st = INTEGER(status_out);

  std::vector<double> buf(b.kmax);
  std::vector<int> items(J);
  for (int i = 0; i < N; ++i) {
    const int* x = &xr[static_cast<size_t>(i) * J];
    int nobs = 0, score = 0, top = 0;
    bool all_low = true, all_high = true;
    for (int j = 0; j < J; ++j) {
      if (x[j] == NA_INTEGER) continue;
      items[nobs++] = j;
      score += x[j];
      top += b.ncat[j] - 1;
      if (x[j] != 0) all_low = false;
      if (x[j] != b.ncat[j] - 1) all_high = false;
    }
    ns[i] = 0;
    if (nobs == 0) {
      th[i] = NA_REAL; se[i] = NA_REAL; st[i] = ST_NO_RESPONSE;
      continue;
    }
    if ((m == M_MLE || m == M_ROBUST) && (all_low || all_high)) {
      th[i] = all_low ? R_NegInf : R_PosInf; se[i] = NA_REAL; st[i] = ST_EXTREME;
      continue;
    }
    const Person p = {b, x, items.data(), nobs, m, pmu[i], ps2[i], H, buf.data()};
    double flo, fhi, D;
    p.eval(lo0, flo, D);
    p.eval(hi0, fhi, D);
    double t;
    if (!(flo > 0.0)) {
      t = lo0; st[i] = ST_BOUNDARY;
    } else if (!(fhi < 0.0)) {
      t = hi0; st[i] = ST_BOUNDARY;
    } else {
      t = pst[i];
      if (ISNAN(t)) {
        const double r = (score + 0.5) / (top + 1.0);
        t = std::log(r / (1.0 - r));
      }
      bool conv;
      ns[i] = safe_newton(p, t, lo0, hi0, exac, maxsteps, conv);
      st[i] = conv ? ST_OK : ST_NOT_CONVERGED;
    }
    th[i] = t;
    se[i] = p.se(t);
  }
  return List::create(_["theta"] = theta_out, _["se"] = se_out,
                      _["nsteps"] = nsteps_out, _["status"] = status_out);
}

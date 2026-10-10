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

// Log category probabilities, in the layout of pp_catprob_cpp()
// [[Rcpp::export]]
NumericMatrix pp_logcatprob_cpp(List bank, NumericVector theta) {
  const ItemBank b = make_bank(bank);
  const int N = theta.size();
  int ntot = 0;
  for (int j = 0; j < b.n; ++j) ntot += b.ncat[j];
  NumericMatrix out(N, ntot);
  std::vector<double> lp(b.kmax);
  for (int i = 0; i < N; ++i) {
    int col = 0;
    for (int j = 0; j < b.n; ++j) {
      log_cat_probs(b, j, theta[i], lp.data());
      for (int k = 0; k < b.ncat[j]; ++k) out(i, col++) = lp[k];
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

// The responses person by person (row-major copy of X), checked once. Item
// skip (0-based, -1 = none) is set to NA, as in a jackknife subsample.
static std::vector<int> response_rows(const ItemBank& b, const IntegerMatrix& X, int skip) {
  const int N = X.nrow(), J = X.ncol();
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
  return xr;
}

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
  const std::vector<int> xr = response_rows(b, X, skip_item - 1);
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

// --- posterior on a grid: EAP, posterior SD and plausible values ----------

// Grid constants (step 5 of the guide). Stage 1 is one grid for all persons:
// the prior means +- GRID_SPAN prior SDs with GRID_H times the smallest prior
// SD between the nodes. A person goes to stage 2 if the posterior SD is below
// STAGE2_SD node distances (the uniform jitter of the PV draws would then add
// more than 1/192 of the variance) or if an end node has a weight above EDGE_W
// (posterior mass beyond the grid). Stage 2 uses a window of +- WIN Laplace
// SDs around the posterior mode with WIN_NODES nodes or more.
static const double GRID_H = 0.05, GRID_SPAN = 6.0, STAGE2_SD = 4.0, EDGE_W = 1e-9, WIN = 10.0;
static const int GRID_QMIN = 61, GRID_QMAX = 2001, WIN_NODES = 201;

// log P_jk on the nodes g_q = lo + q h (q = 0 .. Q-1), stored as
// tab[(cum[j] + k) * Q + q]: the sum over the items of a person runs over
// contiguous rows
struct LogTable {
  double lo, h;
  int Q;
  std::vector<int> cum;        // number of categories of the items before item j
  std::vector<double> tab;
};

static LogTable make_table(const ItemBank& b, double lo, double h, int Q) {
  LogTable t;
  t.lo = lo; t.h = h; t.Q = Q;
  t.cum.resize(b.n);
  int tot = 0;
  for (int j = 0; j < b.n; ++j) { t.cum[j] = tot; tot += b.ncat[j]; }
  t.tab.resize(static_cast<size_t>(tot) * Q);
  std::vector<double> lp(b.kmax);
  for (int q = 0; q < Q; ++q) {
    const double g = lo + q * h;
    for (int j = 0; j < b.n; ++j) {
      log_cat_probs(b, j, g, lp.data());
      for (int k = 0; k < b.ncat[j]; ++k) t.tab[static_cast<size_t>(t.cum[j] + k) * Q + q] = lp[k];
    }
  }
  return t;
}

// The posterior of one person on n equally spaced nodes g0 + q h
struct Post { double mean, sd, edge; };

// On input w holds the log-likelihood on the nodes. Adds the log prior
// N(mu, s2), normalises by log-sum-exp and returns mean, SD and the larger end
// weight; w then holds the normalised weights.
static Post normalise(double* w, int n, double g0, double h, double mu, double s2) {
  double mx = R_NegInf;
  for (int q = 0; q < n; ++q) {
    const double d = g0 + q * h - mu;
    w[q] -= d * d / (2.0 * s2);
    mx = std::max(mx, w[q]);
  }
  double sum = 0.0;
  for (int q = 0; q < n; ++q) { w[q] = std::exp(w[q] - mx); sum += w[q]; }
  double m = 0.0;
  for (int q = 0; q < n; ++q) { w[q] /= sum; m += w[q] * (g0 + q * h); }
  double v = 0.0;
  for (int q = 0; q < n; ++q) { const double d = g0 + q * h - m; v += w[q] * d * d; }
  const Post r = {m, std::sqrt(v), std::max(w[0], w[n - 1])};
  return r;
}

// Posterior on the nodes q0 .. q0 + n - 1 of a table
static Post post_table(const LogTable& t, const Person& p, int q0, int n, double* w) {
  std::fill(w, w + n, 0.0);
  for (int k = 0; k < p.nobs; ++k) {
    const int j = p.items[k];
    const double* row = &t.tab[static_cast<size_t>(t.cum[j] + p.x[j]) * t.Q + q0];
    for (int q = 0; q < n; ++q) w[q] += row[q];
  }
  return normalise(w, n, t.lo + q0 * t.h, t.h, p.mu, p.s2);
}

// Posterior on n nodes from g0, with the item kernel instead of a table
static Post post_direct(const Person& p, double g0, double h, int n, double* w) {
  for (int q = 0; q < n; ++q) {
    const double g = g0 + q * h;
    double ll = 0.0;
    for (int k = 0; k < p.nobs; ++k) {
      const int j = p.items[k];
      ll += eval_item<true>(p.b, j, p.x[j], g, p.buf).ll;
    }
    w[q] = ll;
  }
  return normalise(w, n, g0, h, p.mu, p.s2);
}

// npv draws from the discretised posterior: a node by its weight (inverse
// CDF), then uniformly within its cell. out is the person's row of an N x npv
// matrix (stride N).
static void draw_pv(const double* w, int n, double g0, double h, int npv, double* out, int N) {
  for (int d = 0; d < npv; ++d) {
    const double u = unif_rand();
    double c = 0.0;
    int q = 0;
    for (; q < n - 1; ++q) {
      c += w[q];
      if (u < c) break;
    }
    out[static_cast<size_t>(d) * N] = g0 + (q + unif_rand() - 0.5) * h;
  }
}

// EAP and posterior SD of each person on a grid in two stages (see the
// constants above); npv > 0 also draws npv plausible values per person. X
// holds the responses (NA = not answered), mu/sigma2 the normal prior of each
// person. Returns eap, psd, stage (1 or 2) and draws (N x npv). The draws are
// made for the stage-1 persons first, then for the stage-2 persons, so they
// are reproducible with set.seed(). max_table (number of doubles) limits the
// stage-2 table; above it the windows are evaluated with the kernel directly
// (a parameter only so the tests can reach this path).
// [[Rcpp::export]]
List pp_posterior_cpp(List bank, IntegerMatrix X, NumericVector mu, NumericVector sigma2,
                      int npv = 0, double max_table = 16777216.0) {
  const ItemBank b = make_bank(bank);
  const int N = X.nrow(), J = X.ncol();
  if (J != b.n) stop("X must have one column per item");
  if (mu.size() != N || sigma2.size() != N) stop("mu and sigma2 must have one entry per person");
  if (npv < 0) stop("npv must be >= 0");
  for (int i = 0; i < N; ++i) {
    if (!R_finite(mu[i]) || !R_finite(sigma2[i]) || !(sigma2[i] > 0)) {
      stop("mu must be finite and sigma2 finite and > 0 (person %d)", i + 1);
    }
  }
  const std::vector<int> xr = response_rows(b, X, -1);

  NumericVector eap(N), psd(N);
  IntegerVector stage(N);
  NumericMatrix draws(N, npv);
  List out = List::create(_["eap"] = eap, _["psd"] = psd, _["stage"] = stage, _["draws"] = draws);
  if (N == 0) return out;
  double* pd = REAL(draws);
  int ncat_all = 0;
  for (int j = 0; j < J; ++j) ncat_all += b.ncat[j];

  std::vector<double> buf(b.kmax);
  std::vector<int> items(J);
  // person i with its answered items (in items) and its prior; method MAP for
  // the posterior mode in stage 2
  auto person = [&](int i) {
    const int* x = &xr[static_cast<size_t>(i) * J];
    int nobs = 0;
    for (int j = 0; j < J; ++j) if (x[j] != NA_INTEGER) items[nobs++] = j;
    const Person p = {b, x, items.data(), nobs, M_MAP, mu[i], sigma2[i], 0.0, buf.data()};
    return p;
  };

  // stage 1: one grid for all persons
  double lo = R_PosInf, hi = R_NegInf, smin = R_PosInf;
  for (int i = 0; i < N; ++i) {
    const double s = std::sqrt(sigma2[i]);
    lo = std::min(lo, mu[i] - GRID_SPAN * s);
    hi = std::max(hi, mu[i] + GRID_SPAN * s);
    smin = std::min(smin, s);
  }
  const double qd = std::ceil((hi - lo) / (GRID_H * smin)) + 1.0;
  const int Q = static_cast<int>(std::min<double>(GRID_QMAX, std::max<double>(GRID_QMIN, qd)));
  const double h = (hi - lo) / (Q - 1);
  const LogTable t1 = make_table(b, lo, h, Q);

  std::vector<double> w(std::max(Q, GRID_QMAX + 2));
  std::vector<int> flagged;
  std::vector<double> guess;
  for (int i = 0; i < N; ++i) {
    const Person p = person(i);
    const Post r = post_table(t1, p, 0, Q, w.data());
    if (r.sd >= STAGE2_SD * h && !(r.edge > EDGE_W)) {
      eap[i] = r.mean; psd[i] = r.sd; stage[i] = 1;
      if (npv > 0) draw_pv(w.data(), Q, lo, h, npv, pd + i, N);
    } else {
      flagged.push_back(i);
      guess.push_back(r.mean);
    }
  }
  const int nf = flagged.size();
  if (nf == 0) return out;

  // stage 2: centre = posterior mode (root of the MAP equation, bracket
  // widened until it changes sign), scale = Laplace SD 1/sqrt(I + 1/sigma2)
  std::vector<double> cen(nf), sc(nf);
  for (int f = 0; f < nf; ++f) {
    const Person p = person(flagged[f]);
    double a = lo, c = hi, fa, fc, D;
    p.eval(a, fa, D);
    for (int r = 0; !(fa > 0.0) && r < 60; ++r) { a -= c - a; p.eval(a, fa, D); }
    p.eval(c, fc, D);
    for (int r = 0; !(fc < 0.0) && r < 60; ++r) { c += c - a; p.eval(c, fc, D); }
    double t = std::min(c, std::max(a, guess[f]));
    bool conv;
    safe_newton(p, t, a, c, 1e-10, 500, conv);
    cen[f] = t;
    sc[f] = p.se(t);
  }
  // one fine table over all windows with a tenth of the smallest scale
  // between the nodes, if it fits into max_table
  double lo2 = R_PosInf, hi2 = R_NegInf, smin2 = R_PosInf;
  for (int f = 0; f < nf; ++f) {
    lo2 = std::min(lo2, cen[f] - WIN * sc[f]);
    hi2 = std::max(hi2, cen[f] + WIN * sc[f]);
    smin2 = std::min(smin2, sc[f]);
  }
  const double h2 = 2.0 * WIN * smin2 / (WIN_NODES - 1);
  const double q2d = std::ceil((hi2 - lo2) / h2) + 1.0;
  const bool use_table = q2d * ncat_all <= max_table;
  LogTable t2;
  if (use_table) t2 = make_table(b, lo2, h2, static_cast<int>(q2d));

  for (int f = 0; f < nf; ++f) {
    const int i = flagged[f];
    const Person p = person(i);
    const double half = WIN * sc[f];
    const double nodes = std::ceil(2.0 * half / h2) + 1.0;
    double g0, hw;
    int n;
    Post r;
    if (use_table && nodes <= GRID_QMAX) {
      const int q0 = std::max(0, static_cast<int>(std::floor((cen[f] - half - lo2) / h2)));
      n = std::min(static_cast<int>(nodes) + 1, t2.Q - q0);
      g0 = lo2 + q0 * h2; hw = h2;
      r = post_table(t2, p, q0, n, w.data());
    } else {
      n = WIN_NODES; hw = 2.0 * half / (n - 1); g0 = cen[f] - half;
      r = post_direct(p, g0, hw, n, w.data());
    }
    // a window that cuts off posterior mass is widened, same node distance
    double wide = half;
    for (int k = 0; r.edge > EDGE_W && k < 5; ++k) {
      wide *= 2.0;
      n = static_cast<int>(std::ceil(2.0 * wide / hw)) + 1;
      if (static_cast<int>(w.size()) < n) w.resize(n);
      g0 = r.mean - wide;
      r = post_direct(p, g0, hw, n, w.data());
    }
    eap[i] = r.mean; psd[i] = r.sd; stage[i] = 2;
    if (npv > 0) draw_pv(w.data(), n, g0, hw, npv, pd + i, N);
  }
  return out;
}

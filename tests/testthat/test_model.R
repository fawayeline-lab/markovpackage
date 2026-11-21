test_that("each row of P sums to 1 and P is >= 0 (all scenarios)", {
  for (per in names(params$periods)) for (c in params$tellers) {
    P <- build_P(params$K, c, params$s,
                 params$periods[[per]]$lambda_priority, params$periods[[per]]$lambda_ordinary)
    expect_equal(unname(rowSums(P)), rep(1, nrow(P)), tolerance = 1e-12)
    expect_true(all(P >= 0))
  }
})

test_that("pi P = pi, pi >= 0, sum 1, and agreement with the eigenvector", {
  P <- build_P(params$K, 2, params$s, 0.10, 0.45)
  pv <- stationary_distribution(P)
  expect_equal(sum(pv), 1, tolerance = 1e-12)
  expect_true(all(pv >= -1e-12))
  expect_equal(as.numeric(pv %*% P), unname(pv), tolerance = 1e-10)
  expect_equal(unname(pv), stationary_eigenvector(P), tolerance = 1e-8)
})

test_that("convergence: pi(n) -> pi (irreducible and aperiodic chain)", {
  P <- build_P(params$K, 2, params$s, 0.05, 0.20)
  pv <- stationary_distribution(P)
  v <- distribution_after_n(P, c(1, rep(0, nrow(P) - 1)), 2000)
  expect_equal(v, unname(pv), tolerance = 1e-8)
})

test_that("one-class case: the general model equals the simple chain", {
  Pg <- build_P(2, 1, 0.4, 0, 0.3)
  Ps <- build_P_simple(2, 0.3, 0.4)
  expect_equal(as.vector(Pg), as.vector(Ps), tolerance = 1e-12)
})

test_that("Monte Carlo simulation is close to the theoretical computation", {
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- build_P(K, c, params$s, lp, lo)
  pv <- stationary_distribution(P)
  sim <- simulate_bank(K, c, params$s, lp, lo, steps = 300000, seed = 1, warmup = 2000)
  pis <- frequencies_in_order(sim, attr(P, "states"))
  expect_lt(max(abs(pis - pv)), 0.01)
  ind <- compute_indicators(P, pv, K, c, params$s, lp, lo)
  expect_equal(sim$throughput_o, ind$lambda_eff_o, tolerance = 0.03)
  expect_equal(sim$throughput_p, ind$lambda_eff_p, tolerance = 0.08)
})

test_that("Little: simulated L = effective throughput * computed mean time W", {
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- build_P(K, c, params$s, lp, lo)
  ind <- compute_indicators(P, stationary_distribution(P), K, c, params$s, lp, lo)
  sim <- simulate_bank(K, c, params$s, lp, lo, steps = 300000, seed = 2, warmup = 2000)
  st <- attr(P, "states")
  L_sim <- sum(sim$freq[cbind(st$a + 1, st$b + 1)] * (st$a + st$b))
  lam_sim <- sim$throughput_p + sim$throughput_o      # measured effective throughput
  W_theo <- (ind$L_p + ind$L_o) / (ind$lambda_eff_p + ind$lambda_eff_o)
  expect_equal(L_sim, lam_sim * W_theo, tolerance = 0.03)
})

test_that("priority customers wait less than ordinary customers", {
  P <- build_P(params$K, 1, params$s, 0.10, 0.45)
  ind <- compute_indicators(P, stationary_distribution(P), params$K, 1, params$s, 0.10, 0.45)
  expect_lt(ind$Wq_p, ind$Wq_o)
})


test_that("customer-by-customer simulation: waiting times close to the computation (with interruption)", {
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  T <- 60000
  sim <- simulate_customers(K, c, params$s, rep(lp, T), rep(lo, T), seed = 3, interruption = TRUE)
  cl <- sim$customers[sim$customers$arrival > 2000 & !is.na(sim$customers$start), ]
  P <- build_P(K, c, params$s, lp, lo)
  ind <- compute_indicators(P, stationary_distribution(P), K, c, params$s, lp, lo)
  expect_equal(mean(cl$wait[cl$type == "ordinary"]), ind$Wq_o, tolerance = 0.12)
  expect_equal(mean(!sim$customers$admitted[sim$customers$arrival > 2000]), ind$p_full, tolerance = 0.12)
})

test_that("day: the rules are respected", {
  profile <- day_profile(params)
  sim <- simulate_customers(params$K, 1, params$s, profile$lp, profile$lo, seed = 1)
  cl <- sim$customers
  expect_equal(length(profile$lp), 480)
  ok <- !is.na(cl$start)
  expect_true(all(cl$start[ok] >= cl$arrival[ok]))     # nobody starts before arriving
  expect_true(all(sim$present <= params$K))            # never more than K customers
  expect_true(all(is.na(cl$start[!cl$admitted])))      # a rejected customer is never served
})

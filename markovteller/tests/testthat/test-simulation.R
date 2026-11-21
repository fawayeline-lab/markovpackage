test_that("Monte Carlo simulation is close to the theoretical computation", {
  params <- load_parameters()
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- build_P(K, c, params$s, lp, lo)
  pv <- stationary_distribution(P)
  sim <- simulate_bank(K, c, params$s, lp, lo, steps = 120000, seed = 1, warmup = 2000)
  pis <- frequencies_in_order(sim, attr(P, "states"))
  expect_lt(max(abs(pis - pv)), 0.015)
  ind <- compute_indicators(P, pv, K, c, params$s, lp, lo)
  expect_equal(sim$throughput_o, ind$lambda_eff_o, tolerance = 0.05)
})

test_that("Little: simulated L = measured effective throughput * computed W", {
  params <- load_parameters()
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- build_P(K, c, params$s, lp, lo)
  ind <- compute_indicators(P, stationary_distribution(P), K, c, params$s, lp, lo)
  sim <- simulate_bank(K, c, params$s, lp, lo, steps = 120000, seed = 2, warmup = 2000)
  st <- attr(P, "states")
  L_sim <- sum(sim$freq[cbind(st$a + 1, st$b + 1)] * (st$a + st$b))
  W_theo <- (ind$L_p + ind$L_o) / (ind$lambda_eff_p + ind$lambda_eff_o)
  expect_equal(L_sim, (sim$throughput_p + sim$throughput_o) * W_theo, tolerance = 0.05)
})

test_that("the simulation is reproducible with the same seed", {
  a <- simulate_bank(3, 1, 0.4, 0, 0.3, steps = 3000, seed = 7)
  b <- simulate_bank(3, 1, 0.4, 0, 0.3, steps = 3000, seed = 7)
  expect_identical(a, b)
})

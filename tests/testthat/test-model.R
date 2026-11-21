params <- load_parameters()

test_that("each row of P sums to 1 and P is >= 0 (all scenarios)", {
  for (per in names(params$periods)) for (c in params$tellers) {
    P <- build_P(params$K, c, params$s,
                 params$periods[[per]]$lambda_priority, params$periods[[per]]$lambda_ordinary)
    expect_equal(unname(rowSums(P)), rep(1, nrow(P)), tolerance = 1e-12)
    expect_true(all(P >= 0))
  }
})

test_that("the number of states is (K+1)(K+2)/2 with priority customers", {
  P <- build_P(params$K, 2, params$s, 0.10, 0.45)
  expect_equal(nrow(P), (params$K + 1) * (params$K + 2) / 2)
})

test_that("pi P = pi, pi >= 0, sum 1, and agreement with the eigenvector", {
  P <- build_P(params$K, 2, params$s, 0.10, 0.45)
  pv <- stationary_distribution(P)
  expect_equal(sum(pv), 1, tolerance = 1e-12)
  expect_true(all(pv >= -1e-12))
  expect_equal(as.numeric(pv %*% P), unname(pv), tolerance = 1e-10)
  expect_equal(unname(pv), stationary_eigenvector(P), tolerance = 1e-8)
})

test_that("3-state example: pi = (56, 60, 27) / 143", {
  P <- build_P_simple(2, 0.3, 0.4)
  expect_equal(unname(stationary_distribution(P)), c(56, 60, 27) / 143, tolerance = 1e-10)
})

test_that("convergence: pi(n) tends to pi", {
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

test_that("priority customers wait less than ordinary customers", {
  P <- build_P(params$K, 1, params$s, 0.10, 0.45)
  ind <- compute_indicators(P, stationary_distribution(P), params$K, 1, params$s, 0.10, 0.45)
  expect_lt(ind$Wq_p, ind$Wq_o)
})

test_that("run_scenarios returns one row per period and number of tellers, and more tellers = less waiting", {
  sc <- run_scenarios(params)
  expect_equal(nrow(sc), 2 * length(params$tellers))
  peak <- sc[sc$period == "peak", ]
  expect_true(all(diff(peak$Wq_ordinary) < 0))
})

test_that("invalid parameters are rejected", {
  expect_error(build_P(8, 5, 1.5, 0.1, 0.45))
  expect_error(build_P(8, 1, 0.25, 0.7, 0.45))
})

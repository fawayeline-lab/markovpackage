# =====================================================================
# model.R: construction of the transition matrix P and related computations
# Model: bank with c tellers, capacity K, two customer classes
# (priority and ordinary). Time step = 1 minute.
# State = (a, b): a priority and b ordinary customers present (a + b <= K).
# =====================================================================

load_parameters <- function(path = "data/parameters.json") {
  jsonlite::fromJSON(path)
}

# List of states (a, b). If lp = 0 there are no priority customers: a = 0.
build_states <- function(K, with_priority = TRUE) {
  amax <- if (with_priority) K else 0
  states <- do.call(rbind, lapply(0:amax, function(a) data.frame(a = a, b = 0:(K - a))))
  rownames(states) <- NULL
  states
}

# One-step transition matrix (1 minute).
# Rules (see the model documentation):
#  - at most 1 arrival: priority (prob. lp), ordinary (lo), otherwise none;
#  - an arrival is refused if a + b = K at the start of the step;
#  - each busy teller finishes its service with probability s,
#    independently of the others (there are min(a + b, c) busy tellers);
#  - priority customers are served first; arrivals and departures are independent.
build_P <- function(K, c, s, lp, lo) {
  stopifnot(lp >= 0, lo >= 0, lp + lo <= 1, s > 0, s <= 1, c >= 1)
  states <- build_states(K, lp > 0)
  n <- nrow(states)
  idx <- matrix(NA_integer_, K + 1, K + 1)
  for (i in seq_len(n)) idx[states$a[i] + 1, states$b[i] + 1] <- i
  P <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))

  for (i in seq_len(n)) {
    a <- states$a[i]; b <- states$b[i]
    sp <- min(a, c)                 # tellers busy with priority customers
    so <- min(b, c - sp)            # tellers busy with ordinary customers
    # distribution of the number of finished services: each busy teller
    # finishes with probability s, independently of the others (binomial)
    dp <- stats::dbinom(0:sp, sp, s)
    do <- stats::dbinom(0:so, so, s)
    # arrival distribution: (none, priority, ordinary)
    arr <- if (a + b == K) c(1, 0, 0) else c(1 - lp - lo, lp, lo)
    for (x in 1:3) for (xp in 0:sp) for (yo in 0:so) {
      pr <- arr[x] * dp[xp + 1] * do[yo + 1]
      if (pr == 0) next
      a2 <- a + (x == 2) - xp
      b2 <- b + (x == 3) - yo
      j <- idx[a2 + 1, b2 + 1]
      P[i, j] <- P[i, j] + pr
    }
  }
  attr(P, "states") <- states
  P
}

# Small chain with one class and one teller (3-state example done by hand)
build_P_simple <- function(K, lambda, s) {
  P <- matrix(0, K + 1, K + 1, dimnames = list(0:K, 0:K))
  for (i in 0:K) {
    up <- if (i < K) lambda * (1 - (if (i > 0) s else 0)) else 0
    dn <- if (i > 0) s * (1 - (if (i < K) lambda else 0)) else 0
    if (i < K) P[i + 1, i + 2] <- up
    if (i > 0) P[i + 1, i] <- dn
    P[i + 1, i + 1] <- 1 - up - dn
  }
  P
}

# Stationary distribution: solve pi (P - I) = 0 with sum(pi) = 1.
stationary_distribution <- function(P) {
  n <- nrow(P)
  A <- t(P) - diag(n)
  A[n, ] <- 1             # replace the last equation by sum = 1
  b <- c(rep(0, n - 1), 1)
  pi_vec <- solve(A, b)
  names(pi_vec) <- rownames(P)
  pi_vec
}

# Same result with the left eigenvector (eigenvalue 1), as a check.
stationary_eigenvector <- function(P) {
  e <- eigen(t(P))
  v <- Re(e$vectors[, which.min(abs(e$values - 1))])
  v / sum(v)
}

# Performance indicators (times in minutes because 1 step = 1 minute)
compute_indicators <- function(P, pi_vec, K, c, s, lp, lo) {
  st <- attr(P, "states")
  a <- st$a; b <- st$b
  serv_p <- pmin(a, c)
  serv_o <- pmin(b, c - serv_p)
  p_full <- sum(pi_vec[a + b == K])
  lam_p <- lp * (1 - p_full)       # effective throughput of priority customers
  lam_o <- lo * (1 - p_full)
  Lq_p <- sum(pi_vec * (a - serv_p))
  Lq_o <- sum(pi_vec * (b - serv_o))
  L_p <- sum(pi_vec * a)
  L_o <- sum(pi_vec * b)
  list(
    p_full = p_full,
    L = L_p + L_o, L_p = L_p, L_o = L_o,
    Lq_p = Lq_p, Lq_o = Lq_o,
    lambda_eff_p = lam_p, lambda_eff_o = lam_o,
    W_p = if (lam_p > 0) L_p / lam_p else NA_real_,           # Little
    W_o = if (lam_o > 0) L_o / lam_o else NA_real_,
    Wq_p = if (lam_p > 0) Lq_p / lam_p else NA_real_,
    Wq_o = if (lam_o > 0) Lq_o / lam_o else NA_real_,
    occupancy = sum(pi_vec * pmin(a + b, c)) / c
  )
}

# Distribution after n steps: pi(n) = pi(0) P^n
distribution_after_n <- function(P, pi0, n) {
  v <- pi0
  for (k in seq_len(n)) v <- v %*% P
  as.numeric(v)
}

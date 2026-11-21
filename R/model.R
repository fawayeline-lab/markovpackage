#' List of the states (a, b) of the model
#'
#' A state is a pair (a, b): `a` priority customers and `b` ordinary
#' customers present in the bank, with `a + b <= K`.
#'
#' @param K Maximum capacity of the bank (number of customers).
#' @param with_priority If `FALSE`, there are no priority customers (a = 0).
#' @return A data.frame with columns `a` and `b`, one row per state.
#' @examples
#' build_states(2)
#' @export
build_states <- function(K, with_priority = TRUE) {
  amax <- if (with_priority) K else 0
  states <- do.call(rbind, lapply(0:amax, function(a) data.frame(a = a, b = 0:(K - a))))
  rownames(states) <- NULL
  states
}

#' Transition matrix of the bank (one step = 1 minute)
#'
#' Rules of the model: at most one arrival per minute (priority with
#' probability `lp`, ordinary with probability `lo`), refused if `a + b = K`;
#' each busy teller (there are `min(a + b, c)` of them) finishes its service
#' with probability `s`, independently of the others; priority customers are
#' served first; arrivals and departures are independent.
#'
#' Each row of the matrix sums to 1, because it is the product of an arrival
#' distribution and binomial departure distributions, each summing to 1.
#'
#' @param K Maximum capacity (positive integer).
#' @param c Number of tellers.
#' @param s Probability that a teller finishes a service in one minute.
#' @param lp Probability that a priority customer arrives in one minute.
#' @param lo Probability that an ordinary customer arrives in one minute.
#' @return A stochastic matrix. Its `"states"` attribute holds the data.frame
#'   of states (see [build_states()]).
#' @examples
#' P <- build_P(K = 3, c = 1, s = 0.4, lp = 0, lo = 0.3)
#' rowSums(P)
#' @export
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

#' Simple transition matrix: one customer class, one teller
#'
#' Used as a worked example by hand (for instance 3 states with `K = 2`).
#'
#' @param K Maximum capacity.
#' @param lambda Probability of an arrival per minute.
#' @param s Probability that the service finishes per minute.
#' @return A stochastic matrix of size `(K + 1) x (K + 1)`.
#' @examples
#' build_P_simple(2, 0.3, 0.4)
#' @export
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

#' Stationary distribution of a Markov chain
#'
#' Solves `pi (P - I) = 0` with `sum(pi) = 1`. It is unique if the chain is
#' finite and irreducible.
#'
#' @param P Transition matrix (stochastic, square).
#' @return A named vector of probabilities summing to 1.
#' @examples
#' P <- build_P_simple(2, 0.3, 0.4)
#' stationary_distribution(P)
#' @export
stationary_distribution <- function(P) {
  n <- nrow(P)
  A <- t(P) - diag(n)
  A[n, ] <- 1
  b <- c(rep(0, n - 1), 1)
  pi_vec <- solve(A, b)
  names(pi_vec) <- rownames(P)
  pi_vec
}

#' Stationary distribution by eigenvector (check)
#'
#' Same result as [stationary_distribution()], obtained with the left
#' eigenvector associated with the eigenvalue 1.
#'
#' @inheritParams stationary_distribution
#' @return A vector of probabilities summing to 1.
#' @examples
#' stationary_eigenvector(build_P_simple(2, 0.3, 0.4))
#' @export
stationary_eigenvector <- function(P) {
  e <- eigen(t(P))
  v <- Re(e$vectors[, which.min(abs(e$values - 1))])
  v / sum(v)
}

#' Performance indicators of the bank
#'
#' Times are in minutes because one step is one minute. Mean times use
#' Little's law, `L = lambda_eff * W`.
#'
#' @param P Matrix returned by [build_P()].
#' @param pi_vec Stationary distribution (see [stationary_distribution()]).
#' @param K,c,s,lp,lo Parameters used to build `P`.
#' @return A list: `p_full`, `L`, `L_p`, `L_o`, `Lq_p`, `Lq_o`,
#'   `lambda_eff_p`, `lambda_eff_o`, `W_p`, `W_o`, `Wq_p`, `Wq_o`
#'   (waiting times in the queue) and `occupancy`.
#' @examples
#' P <- build_P(8, 2, 0.25, 0.10, 0.45)
#' ind <- compute_indicators(P, stationary_distribution(P), 8, 2, 0.25, 0.10, 0.45)
#' ind$Wq_o
#' @export
compute_indicators <- function(P, pi_vec, K, c, s, lp, lo) {
  st <- attr(P, "states")
  a <- st$a
  b <- st$b
  serv_p <- pmin(a, c)
  serv_o <- pmin(b, c - serv_p)
  p_full <- sum(pi_vec[a + b == K])
  lam_p <- lp * (1 - p_full)
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
    W_p = if (lam_p > 0) L_p / lam_p else NA_real_,
    W_o = if (lam_o > 0) L_o / lam_o else NA_real_,
    Wq_p = if (lam_p > 0) Lq_p / lam_p else NA_real_,
    Wq_o = if (lam_o > 0) Lq_o / lam_o else NA_real_,
    occupancy = sum(pi_vec * pmin(a + b, c)) / c
  )
}

#' Distribution after n steps: pi(n) = pi(0) P^n
#'
#' @param P Transition matrix.
#' @param pi0 Initial distribution (row vector summing to 1).
#' @param n Number of steps (minutes).
#' @return The vector of probabilities after `n` steps.
#' @examples
#' P <- build_P_simple(2, 0.3, 0.4)
#' distribution_after_n(P, c(1, 0, 0), 10)
#' @export
distribution_after_n <- function(P, pi0, n) {
  v <- pi0
  for (k in seq_len(n)) v <- v %*% P
  as.numeric(v)
}

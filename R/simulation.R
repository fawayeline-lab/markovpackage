#' One simulation step (internal)
#'
#' Each busy teller finishes its service with probability `s`.
#'
#' @noRd
simulation_step <- function(a, b, K, c, s, lp, lo, u_arrival) {
  full <- (a + b) >= K
  sp <- min(a, c)
  so <- min(b, c - sp)
  xp <- if (sp > 0) sum(stats::runif(sp) < s) else 0L
  yo <- if (so > 0) sum(stats::runif(so) < s) else 0L
  arr <- if (u_arrival < lp) 1L else if (u_arrival < lp + lo) 2L else 0L
  list(a = as.integer(a + (arr == 1L && !full) - xp),
       b = as.integer(b + (arr == 2L && !full) - yo),
       arr = arr, full = full)
}

#' Monte Carlo simulation of the bank
#'
#' Simulates the bank minute by minute by drawing arrivals and departures at
#' random (teller by teller), **without using the transition matrix**. If the
#' observed frequencies match the stationary distribution, the matrix is
#' correct.
#'
#' @inheritParams build_P
#' @param steps Number of simulated minutes.
#' @param seed Seed of the random number generator (reproducible results).
#' @param warmup Number of initial minutes that are ignored.
#' @return A list: `freq` (matrix of frequencies per state (a, b)),
#'   `n_steps`, `throughput_p`, `throughput_o` (customers admitted per
#'   minute) and `rejected_p`, `rejected_o` (customers refused per minute).
#' @examples
#' sim <- simulate_bank(3, 1, 0.4, 0, 0.3, steps = 5000)
#' sim$n_steps
#' @export
simulate_bank <- function(K, c, s, lp, lo, steps, seed = 123, warmup = 1000) {
  set.seed(seed)
  ua <- stats::runif(steps)
  a <- 0L
  b <- 0L
  tab <- matrix(0, K + 1, K + 1)
  adm_p <- adm_o <- rej_p <- rej_o <- 0
  nb <- 0
  for (t in seq_len(steps)) {
    in_steady_state <- t > warmup
    if (in_steady_state) {
      tab[a + 1, b + 1] <- tab[a + 1, b + 1] + 1
      nb <- nb + 1
    }
    r <- simulation_step(a, b, K, c, s, lp, lo, ua[t])
    if (r$arr > 0L && in_steady_state) {
      if (r$full) {
        if (r$arr == 1L) rej_p <- rej_p + 1 else rej_o <- rej_o + 1
      } else {
        if (r$arr == 1L) adm_p <- adm_p + 1 else adm_o <- adm_o + 1
      }
    }
    a <- r$a
    b <- r$b
  }
  list(freq = tab / nb, n_steps = nb,
       throughput_p = adm_p / nb, throughput_o = adm_o / nb,
       rejected_p = rej_p / nb, rejected_o = rej_o / nb)
}

#' Simulated frequencies in the order of the matrix states
#'
#' @param sim Result of [simulate_bank()].
#' @param states Data.frame of states, for example `attr(P, "states")`.
#' @return A vector of frequencies, in the same order as the rows of `P`.
#' @examples
#' P <- build_P(3, 1, 0.4, 0, 0.3)
#' sim <- simulate_bank(3, 1, 0.4, 0, 0.3, steps = 5000)
#' frequencies_in_order(sim, attr(P, "states"))
#' @export
frequencies_in_order <- function(sim, states) {
  mapply(function(a, b) sim$freq[a + 1, b + 1], states$a, states$b)
}

#' Minute-by-minute trajectory
#'
#' To visualize the bank filling up and emptying.
#'
#' @inheritParams build_P
#' @param steps Number of minutes.
#' @param seed Seed of the random number generator.
#' @return A data.frame: `t`, `priority`, `ordinary` and `total` (customers
#'   present).
#' @examples
#' head(bank_trajectory(8, 2, 0.25, 0.1, 0.45, steps = 30))
#' @export
bank_trajectory <- function(K, c, s, lp, lo, steps, seed = 42) {
  set.seed(seed)
  ua <- stats::runif(steps)
  a <- 0L
  b <- 0L
  res <- data.frame(t = 0:(steps - 1), priority = integer(steps), ordinary = integer(steps))
  for (t in seq_len(steps)) {
    res$priority[t] <- a
    res$ordinary[t] <- b
    r <- simulation_step(a, b, K, c, s, lp, lo, ua[t])
    a <- r$a
    b <- r$b
  }
  res$total <- res$priority + res$ordinary
  res
}

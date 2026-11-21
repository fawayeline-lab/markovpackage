# =====================================================================
# simulation.R: Monte Carlo simulation, "event by event".
# It DOES NOT USE the matrix P: it draws arrivals and departures
# directly, teller by teller. If it matches the results from P,
# then P is correct.
# =====================================================================

# One simulation step: returns the new state (a, b) and what happened.
# Each busy teller finishes its service with probability s.
simulation_step <- function(a, b, K, c, s, lp, lo, u_arrival) {
  full <- (a + b) >= K
  sp <- min(a, c)                          # tellers busy with priority customers
  so <- min(b, c - sp)                     # tellers busy with ordinary customers
  xp <- if (sp > 0) sum(runif(sp) < s) else 0L   # finished services (priority)
  yo <- if (so > 0) sum(runif(so) < s) else 0L   # finished services (ordinary)
  arr <- if (u_arrival < lp) 1L else if (u_arrival < lp + lo) 2L else 0L
  list(a = as.integer(a + (arr == 1L && !full) - xp),
       b = as.integer(b + (arr == 2L && !full) - yo),
       arr = arr, full = full)
}

simulate_bank <- function(K, c, s, lp, lo, steps, seed = 123, warmup = 1000) {
  set.seed(seed)
  ua <- runif(steps)
  a <- 0L; b <- 0L
  tab <- matrix(0, K + 1, K + 1)      # number of steps spent in each state
  adm_p <- adm_o <- rej_p <- rej_o <- 0
  nb <- 0
  for (t in seq_len(steps)) {
    in_steady_state <- t > warmup
    if (in_steady_state) { tab[a + 1, b + 1] <- tab[a + 1, b + 1] + 1; nb <- nb + 1 }
    r <- simulation_step(a, b, K, c, s, lp, lo, ua[t])
    if (r$arr > 0L && in_steady_state) {
      if (r$full) { if (r$arr == 1L) rej_p <- rej_p + 1 else rej_o <- rej_o + 1 }
      else        { if (r$arr == 1L) adm_p <- adm_p + 1 else adm_o <- adm_o + 1 }
    }
    a <- r$a; b <- r$b
  }
  list(freq = tab / nb, n_steps = nb,
       throughput_p = adm_p / nb, throughput_o = adm_o / nb,
       rejected_p = rej_p / nb, rejected_o = rej_o / nb)
}

# Puts the simulated frequencies in the same state order as the matrix P
frequencies_in_order <- function(sim, states) {
  mapply(function(a, b) sim$freq[a + 1, b + 1], states$a, states$b)
}

# Minute-by-minute trajectory (to visualize the bank filling up and emptying)
bank_trajectory <- function(K, c, s, lp, lo, steps, seed = 42) {
  set.seed(seed)
  ua <- runif(steps)
  a <- 0L; b <- 0L
  res <- data.frame(t = 0:(steps - 1), priority = integer(steps), ordinary = integer(steps))
  for (t in seq_len(steps)) {
    res$priority[t] <- a; res$ordinary[t] <- b
    r <- simulation_step(a, b, K, c, s, lp, lo, ua[t])
    a <- r$a; b <- r$b
  }
  res$total <- res$priority + res$ordinary
  res
}

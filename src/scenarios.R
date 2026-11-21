# =====================================================================
# scenarios.R: 1 to 5 tellers x off-peak / peak hours
# =====================================================================

run_scenarios <- function(params) {
  rows <- list()
  for (per in names(params$periods)) {
    lp <- params$periods[[per]]$lambda_priority
    lo <- params$periods[[per]]$lambda_ordinary
    for (c in params$tellers) {
      P <- build_P(params$K, c, params$s, lp, lo)
      pv <- stationary_distribution(P)
      ind <- compute_indicators(P, pv, params$K, c, params$s, lp, lo)
      rows[[length(rows) + 1]] <- data.frame(
        period = per, tellers = c,
        load = (lp + lo) / (c * params$s),     # arrivals / service capacity
        L = ind$L, Wq_priority = ind$Wq_p, Wq_ordinary = ind$Wq_o,
        W_priority = ind$W_p, W_ordinary = ind$W_o,
        p_full = ind$p_full, occupancy = ind$occupancy)
    }
  }
  do.call(rbind, rows)
}

# Smallest number of tellers such that the mean waiting time of ordinary
# customers does not exceed `threshold` minutes, for each period (off_peak / peak).
required_tellers <- function(sc, threshold) {
  sapply(split(sc, sc$period), function(d) {
    ok <- d$tellers[d$Wq_ordinary <= threshold]
    if (length(ok)) min(ok) else NA_integer_
  })
}

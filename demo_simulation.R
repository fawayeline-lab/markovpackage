# =====================================================================
# demo_simulation.R: ONE DAY AT THE BRANCH, simulated customer by customer.
#
# Monday, from 8:00 to 16:00. Customers arrive at random (more of them
# from 11:00 to 14:00), the tellers serve them, and for EVERY customer we
# record the arrival time, the waiting time and the departure. We compare
# 1 to 5 tellers with exactly the same sequence of arrivals.
#
# RStudio: open this file then click "Source".
# The plots appear in the Plots tab (Zoom button to enlarge them).
# =====================================================================
dir.create("outputs", showWarnings = FALSE); dir.create("figures", showWarnings = FALSE)

# ---- SETTINGS (you can change them) ----
animation <- TRUE          # TRUE: watch the bank fill up live. FALSE: no animation.
speed     <- 0.04          # pause between two frames of the animation, in seconds (smaller = faster)
pause     <- 2             # pause (seconds) after each case, to read the result
pause_between_cases <- FALSE   # TRUE: wait for you to press Enter before moving to the next case
# ----------------------------------------

params <- load_parameters()
K <- params$K
tellers <- params$tellers

# ---------------------------------------------------------------------
# 1. A full day, customer by customer
# ---------------------------------------------------------------------
cat("\n=== 1. Monday, 8:00 to 16:00: 1 to 5 tellers, same customers ===\n")
profile <- day_profile(params)                 # 8:00 - 16:00, peak from 11:00 to 14:00
days <- lapply(tellers, function(c) simulate_customers(K, c, params$s, profile$lp, profile$lo, seed = 2026))
names(days) <- tellers
summary_df <- do.call(rbind, lapply(seq_along(tellers), function(k) summarize_day(days[[k]], tellers[k])))
print(summary_df, row.names = FALSE)
write.csv(summary_df, "outputs/day_summary.csv", row.names = FALSE)
write.csv(days[["4"]]$customers, "outputs/day_customers_4tellers.csv", row.names = FALSE)
cat("\nThe customers of the day with 4 tellers are in outputs/day_customers_4tellers.csv\n",
    "(one row per customer: arrival, start of service, end, wait in minutes).\n", sep = "")
cat("Columns: arrived = customers who arrived; rejected = bank full; wait_* = minutes before reaching a teller.\n",
    "Reading: with 1 teller, customers are rejected (bank full) and the wait explodes at peak time;\n",
    "with 4 tellers, almost everyone is served without waiting.\n", sep = "")

# Plots
draw_compared_days <- function() {
  op <- par(mfrow = c(2, 1)); on.exit(par(op))
  draw_day(days[["1"]], 1, profile, K)
  draw_day(days[["4"]], 4, profile, K)
}
draw_compared_waits <- function() draw_waiting_times(days[["1"]], 1, days[["4"]], 4)
draw_five_days <- function() draw_all_days(days, profile, K, summary_df)

if (interactive()) {
  # CASE BY CASE: we start with 1 teller, then 2, then 3, 4 and 5.
  # For each case: the live animation of the bank, then the plot of the day.
  for (c in tellers) {
    r <- summary_df[summary_df$tellers == c, ]
    cat(sprintf("\n--- CASE %d of %d: %d teller%s ---\n", which(tellers == c), length(tellers), c, ifelse(c > 1, "s", "")))
    if (animation) {
      cat("Animation: the bank fills up live (from 8:00 to 16:00)...\n")
      show_plot(function() animate_day(days[[as.character(c)]], c, profile, K, step = 8, delay = speed))
    }
    show_plot(function() draw_day(days[[as.character(c)]], c, profile, K))
    cat(sprintf("Result: %d customers rejected out of %d, mean wait %s min (maximum %d min).\n",
                r$rejected, r$arrived, format(r$wait_mean, decimal.mark = "."), r$wait_max))
    if (pause_between_cases && c != tail(tellers, 1)) invisible(readline("Press Enter to move to the next case... "))
    else Sys.sleep(pause)
  }
  cat("\n--- To finish: each customer's wait, then the 5 days together ---\n",
      "Arrow ◀ above the plot to go back, Zoom to enlarge.\n", sep = "")
  show_plot(draw_compared_waits)
  show_plot(draw_five_days)                  # last plot: the 5 days at once
}
save_png("figures/fig7_day.png", 1600, 1200, 180, draw_compared_days)
save_png("figures/fig8_waits.png", 1800, 800, 200, draw_compared_waits)
save_png("figures/fig9_five_days.png", 1800, 1500, 200, draw_five_days)

# ---------------------------------------------------------------------
# 2. Check: does the customer-by-customer simulation match the computation?
# ---------------------------------------------------------------------
cat("\n=== 2. Check at peak time (constant arrivals, 100,000 minutes) ===\n")
lp <- params$periods$peak$lambda_priority; lo <- params$periods$peak$lambda_ordinary
T_val <- 100000
check <- do.call(rbind, lapply(tellers, function(c) {
  sim <- simulate_customers(K, c, params$s, rep(lp, T_val), rep(lo, T_val), seed = 5, interruption = TRUE)
  cl <- sim$customers[sim$customers$arrival > 2000, ]
  served <- cl[!is.na(cl$start), ]
  P <- build_P(K, c, params$s, lp, lo)
  ind <- compute_indicators(P, stationary_distribution(P), K, c, params$s, lp, lo)
  data.frame(tellers = c,
             ordinary_measured = round(mean(served$wait[served$type == "ordinary"]), 2),
             ordinary_computed = round(ind$Wq_o, 2),
             priority_measured = round(mean(served$wait[served$type == "priority"]), 2),
             priority_computed = round(ind$Wq_p, 2),
             rejected_measured = round(mean(!cl$admitted), 4),
             full_computed = round(ind$p_full, 4))
}))
print(check, row.names = FALSE)
write.csv(check, "outputs/day_check.csv", row.names = FALSE)
jsonlite::write_json(list(summary = summary_df, check = check, check_steps = T_val, day_seed = 2026),
                     "outputs/day.json", pretty = TRUE, digits = 6, auto_unbox = TRUE)
cat("\nThe 'measured' and 'computed' columns should be close: the customer-by-customer simulation\n",
    "confirms the Markov matrix computation.\n", sep = "")

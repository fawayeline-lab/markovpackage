# =====================================================================
# day.R: a TRUE customer-by-customer simulation.
#
# We run a day at the branch minute by minute. Each customer is tracked
# individually: arrival time, start of service, end of service, waiting
# time. The rules are those of the model (see the model documentation):
#  - at most one arrival per minute (priority with prob. lp[t],
#    ordinary with lo[t]); refused if the bank already holds K customers;
#  - each busy teller finishes its service with probability s every
#    minute;
#  - priority customers go before ordinary ones, in order of arrival.
#  - interruption = FALSE (realistic): a service that has started is never
#    interrupted. interruption = TRUE: a priority customer can take the
#    place of an ordinary customer being served (this is the assumption of
#    the transition matrix P; we use it to compare the simulation with the
#    computation).
# =====================================================================

# lp and lo are vectors: one arrival probability for EACH minute
# (for example higher at peak time).
simulate_customers <- function(K, c, s, lp, lo, seed = 1, interruption = FALSE) {
  set.seed(seed)
  T <- length(lp)
  u <- runif(T)                                   # arrival draws (same sequence for every c)
  type <- integer(T); arrival <- integer(T); start <- rep(NA_integer_, T)
  end <- rep(NA_integer_, T); admitted <- logical(T)
  n <- 0L
  qp <- integer(0); qo <- integer(0)              # waiting queues: priority, ordinary
  server <- integer(c)                            # 0 = free teller, otherwise number of the customer served
  present <- integer(T)                           # customers in the bank at the start of each minute
  for (t in seq_len(T)) {
    occ <- which(server > 0L)
    present[t] <- length(qp) + length(qo) + length(occ)
    already <- present[t]
    if (length(occ)) {                            # end of services
      done <- occ[runif(length(occ)) < s]
      if (length(done)) { end[server[done]] <- t; server[done] <- 0L }
    }
    a <- if (u[t] < lp[t]) 1L else if (u[t] < lp[t] + lo[t]) 2L else 0L   # arrival
    if (a > 0L) {
      n <- n + 1L; type[n] <- a; arrival[n] <- t
      if (already < K) {
        admitted[n] <- TRUE
        if (a == 1L) qp <- c(qp, n) else qo <- c(qo, n)
      }
    }
    for (j in which(server == 0L)) {              # assign the free tellers
      if (length(qp)) { id <- qp[1]; qp <- qp[-1] }
      else if (length(qo)) { id <- qo[1]; qo <- qo[-1] }
      else break
      server[j] <- id; if (is.na(start[id])) start[id] <- t
    }
    if (interruption && length(qp)) {             # a priority customer can interrupt an ordinary one in service
      for (j in which(server > 0L & type[pmax(server, 1L)] == 2L)) {
        if (!length(qp)) break
        qo <- c(server[j], qo)                    # the ordinary customer goes back to the head of its queue
        id <- qp[1]; qp <- qp[-1]
        server[j] <- id; if (is.na(start[id])) start[id] <- t
      }
    }
  }
  customers <- data.frame(
    id = seq_len(n), type = ifelse(type[seq_len(n)] == 1L, "priority", "ordinary"),
    arrival = arrival[seq_len(n)], admitted = admitted[seq_len(n)],
    start = start[seq_len(n)], end = end[seq_len(n)])
  customers$wait <- customers$start - customers$arrival     # minutes spent in the queue before reaching a teller
  list(customers = customers, present = present)
}

# Minute-by-minute arrival probabilities for a day with several periods.
# params: read from data/parameters.json
day_profile <- function(params, opening = 8, closing = 16, peak = c(11, 14)) {
  T <- (closing - opening) * 60
  hour <- opening + (seq_len(T) - 1) / 60
  in_peak <- hour >= peak[1] & hour < peak[2]
  lp <- ifelse(in_peak, params$periods$peak$lambda_priority, params$periods$off_peak$lambda_priority)
  lo <- ifelse(in_peak, params$periods$peak$lambda_ordinary, params$periods$off_peak$lambda_ordinary)
  list(lp = lp, lo = lo, hour = hour, peak = peak, opening = opening)
}

# Summary of a day: how many customers, waiting time, rejections
summarize_day <- function(sim, c) {
  cl <- sim$customers
  served <- cl[!is.na(cl$start), ]
  data.frame(
    tellers = c,
    arrived = nrow(cl),
    rejected = sum(!cl$admitted),
    served = nrow(served),
    wait_mean = if (nrow(served)) round(mean(served$wait), 2) else NA,
    wait_ordinary = round(mean(served$wait[served$type == "ordinary"]), 2),
    wait_priority = round(mean(served$wait[served$type == "priority"]), 2),
    wait_max = max(served$wait))
}

# ---------- Plots ----------
draw_day <- function(sim, c, profile, K, short = FALSE, ylab = TRUE) {
  p <- profile
  cl <- sim$customers
  n_rej <- sum(!cl$admitted)
  wt <- round(mean(cl$wait, na.rm = TRUE), 1)
  op <- par(mar = c(3, if (ylab) 3.8 else 2.4, 3.6, 0.8), cex.axis = 0.95, cex.lab = 1)
  on.exit(par(op))
  x <- p$hour; y <- sim$present
  plot(x, y, type = "n", ylim = c(0, K + 0.6), xlim = range(x), xaxt = "n", las = 1,
       xlab = "", ylab = if (!ylab) "" else if (short) "customers" else "customers in the bank")
  rect(p$peak[1], -1, p$peak[2], K + 1, col = "#0000000F", border = NA)           # grey area: peak
  abline(h = K, lty = 2, col = "#C55A11", lwd = 1.3)                              # capacity
  xn <- c(x[-1], x[length(x)] + 1 / 60)                                           # step curve, with filled area
  xs <- as.vector(rbind(x, xn)); ys <- as.vector(rbind(y, y))
  polygon(c(xs[1], xs, xs[length(xs)]), c(0, ys, 0), col = "#1F4E7924", border = NA)
  lines(xs, ys, col = "#1F4E79", lwd = 2)
  h0 <- p$opening; h1 <- p$opening + length(x) / 60
  axis(1, at = seq(h0, h1, by = 2), labels = sprintf("%d:00", seq(h0, h1, by = 2)))
  title(main = sprintf("%d teller%s", c, ifelse(c > 1, "s", "")), line = 1.7, cex.main = 1.15)
  mtext(sprintf("%d rejected · mean wait %s min", n_rej, format(wt, decimal.mark = ".")),
        side = 3, line = 0.35, cex = 0.85, font = 2, col = if (n_rej > 0) "#C55A11" else "#2E7D6B")
}

draw_waiting_times <- function(sim_a, c_a, sim_b, c_b) {
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), cex.main = 1.15, cex.axis = 0.95); on.exit(par(op))
  m <- max(c(sim_a$customers$wait, sim_b$customers$wait), na.rm = TRUE)
  cases <- list(list(sim_a, c_a), list(sim_b, c_b))
  for (z in cases) {
    w <- z[[1]]$customers$wait[z[[1]]$customers$admitted & !is.na(z[[1]]$customers$wait)]
    hist(w, breaks = seq(0, ceiling(m / 5) * 5 + 5, by = 5), col = "#1F4E79", border = "white",
         main = sprintf("%d teller%s", z[[2]], ifelse(z[[2]] > 1, "s", "")),
         xlab = "wait before reaching a teller (min)", ylab = "number of customers")
  }
}

# ---------------------------------------------------------------------
# Animation: we watch the bank fill up LIVE, minute after minute
# ---------------------------------------------------------------------
time_label <- function(t, opening = 8) {
  m <- opening * 60 + (t - 1)
  sprintf("%d:%02d", m %/% 60, m %% 60)
}

# Who is being served, who is waiting, how many rejected, at minute t
state_at_minute <- function(cl, t) {
  adm <- cl$admitted
  serv <- adm & !is.na(cl$start) & cl$start <= t & (is.na(cl$end) | cl$end > t)
  wait <- adm & cl$arrival <= t & (is.na(cl$start) | cl$start > t)
  list(service = cl[serv, ], waiting = cl[wait, ],
       rejected = sum(!adm & cl$arrival <= t),
       served = sum(adm & !is.na(cl$end) & cl$end <= t))
}

# One frame of the bank at minute t: tellers, queue, counters, curve
draw_branch <- function(sim, c, profile, K, t) {
  e <- state_at_minute(sim$customers, t)
  op <- par(no.readonly = TRUE); on.exit({ layout(1); par(op) })
  layout(matrix(1:2, 2, 1), heights = c(1.35, 1))
  par(mar = c(0.3, 1, 2.8, 1))
  plot(NULL, xlim = c(0, 10), ylim = c(0, 4.2), axes = FALSE, xlab = "", ylab = "",
       main = sprintf("%d teller%s · it is %s", c, ifelse(c > 1, "s", ""), time_label(t, profile$opening)), cex.main = 1.25)
  width <- min(1.35, 6.4 / c)
  text(0.3, 4.0, "TELLERS", adj = 0, cex = 0.85, font = 2, col = "#555555")
  for (j in seq_len(c)) {                                    # the tellers
    x0 <- 0.3 + (j - 1) * (width + 0.12)
    busy <- j <= nrow(e$service)
    rect(x0, 2.75, x0 + width, 3.8, col = if (busy) "#1F4E79" else "#F0F0F0", border = "#1F4E79", lwd = 1.5)
    text(x0 + width / 2, 3.5, if (busy) "busy" else "free", cex = 0.85, font = 2, col = if (busy) "white" else "#888888")
    if (busy) points(x0 + width / 2, 3.05, pch = 19, cex = 1.5,
                     col = if (e$service$type[j] == "priority") "#F4A259" else "white")
  }
  wq <- e$waiting[order(e$waiting$type != "priority", e$waiting$arrival), ]
  text(0.3, 2.45, sprintf("QUEUE (%d)", nrow(wq)), adj = 0, cex = 0.85, font = 2, col = "#555555")
  for (k in seq_len(nrow(wq)))                               # the queue
    rect(0.3 + (k - 1) * 0.6, 1.35, 0.3 + (k - 1) * 0.6 + 0.5, 2.25, border = NA,
         col = if (wq$type[k] == "priority") "#F4A259" else "#7FA6CF")
  inside <- nrow(e$service) + nrow(wq)
  text(9.8, 3.75, sprintf("in the bank: %d / %d", inside, K), adj = 1, cex = 1.15, font = 2)
  text(9.8, 3.15, sprintf("served: %d", e$served), adj = 1, cex = 1.0, col = "#444444")
  text(9.8, 2.55, sprintf("rejected: %d", e$rejected), adj = 1, cex = 1.05,
       col = if (e$rejected > 0) "#C55A11" else "#444444", font = if (e$rejected > 0) 2 else 1)
  rect(0.3, 0.3, 0.6, 0.6, col = "#F4A259", border = NA); text(0.75, 0.45, "priority", adj = 0, cex = 0.8, col = "#555555")
  rect(2.6, 0.3, 2.9, 0.6, col = "#7FA6CF", border = NA); text(3.05, 0.45, "ordinary", adj = 0, cex = 0.8, col = "#555555")
  par(mar = c(3, 4, 0.8, 1), cex.axis = 0.95)                # the curve being built
  plot(profile$hour[seq_len(t)], sim$present[seq_len(t)], type = "s", lwd = 2, col = "#1F4E79", las = 1,
       xlim = range(profile$hour), ylim = c(0, K + 0.6), xaxt = "n", xlab = "", ylab = "customers")
  rect(profile$peak[1], -1, profile$peak[2], K + 1, col = "#0000000F", border = NA)
  abline(h = K, lty = 2, col = "#C55A11")
  axis(1, at = seq(profile$opening, profile$opening + length(profile$hour) / 60, by = 2),
       labels = sprintf("%d:00", seq(profile$opening, profile$opening + length(profile$hour) / 60, by = 2)))
}

# Plays the day in fast forward: one frame every `step` minutes
animate_day <- function(sim, c, profile, K, step = 6, delay = 0.04) {
  T <- length(profile$lp)
  for (t in unique(c(seq(1, T, by = step), T))) {
    dev.hold(); draw_branch(sim, c, profile, K, t); dev.flush()
    Sys.sleep(delay)
  }
  invisible(TRUE)
}

# The 5 days side by side: one plot per number of tellers + a readable summary
draw_all_days <- function(days, profile, K, summary_df) {
  op <- par(mfrow = c(3, 2)); on.exit(par(op))
  par(cex = 1)                                   # larger text than R's automatic setting
  for (k in seq_along(days))
    draw_day(days[[k]], as.integer(names(days)[k]), profile, K, short = TRUE, ylab = (k %% 2 == 1))
  op2 <- par(mar = c(1, 1, 1, 1)); on.exit(par(op2), add = TRUE)
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  text(0.02, 0.99, "Summary of the day", adj = c(0, 1), font = 2, cex = 1.0)
  text(c(0.02, 0.40, 0.66), 0.80, c("Tellers", "Rejected", "Wait"), adj = 0, font = 2, cex = 0.8, col = "#555555")
  segments(0.02, 0.745, 0.98, 0.745, col = "#BBBBBB")
  for (i in seq_len(nrow(summary_df))) {
    y <- 0.67 - (i - 1) * 0.105
    rej <- summary_df$rejected[i] > 0
    text(0.02, y, summary_df$tellers[i], adj = 0, cex = 0.8)
    text(0.40, y, summary_df$rejected[i], adj = 0, cex = 0.8, font = if (rej) 2 else 1, col = if (rej) "#C55A11" else "#2E7D6B")
    text(0.66, y, sprintf("%s min", format(summary_df$wait_mean[i], decimal.mark = ".")), adj = 0, cex = 0.8)
  }
  rect(0.02, 0.07, 0.07, 0.17, col = "#0000000F", border = "#999999")
  text(0.10, 0.12, "grey area: peak hours", adj = 0, cex = 0.7, col = "#555555")
  segments(0.02, 0.01, 0.07, 0.01, lty = 2, col = "#C55A11", lwd = 1.5)
  text(0.10, 0.01, sprintf("orange line: capacity (%d customers)", K), adj = 0, cex = 0.7, col = "#555555")
}

# =====================================================================
# figures.R: plots of the project.
#  - draw_*()  : draw the plot on screen (RStudio Plots tab)
#  - fig_*()   : save the same plot as a PNG in /figures
#  - show_plot(): displays a plot on screen without ever crashing,
#                 even if the Plots pane is too small
# =====================================================================

COL_ACCENT <- "#1F4E79"; COL_2 <- "#C55A11"; COL_GREY <- "#7F7F7F"

# Displays a plot in the Plots tab. If the pane is too small, R refuses
# to draw: we then print a clear message instead of an error.
show_plot <- function(draw) {
  tryCatch({ draw(); invisible(TRUE) },
           error = function(e) {
             message("The plot could not be displayed (the Plots pane is probably too small). ",
                     "Enlarge it (double-click the Plots tab) or open the image in the 'figures' folder.")
             invisible(FALSE)
           })
}

# Saves a plot as a PNG
save_png <- function(path, width, height, res, draw) {
  png(path, width = width, height = height, res = res)
  on.exit(invisible(dev.off()))
  draw()
  invisible(path)
}

# ---------- 1. 3-state diagram ----------
draw_diagram_3states <- function(P) {
  op <- par(mar = c(1, 1, 3, 1)); on.exit(par(op))
  plot(NULL, xlim = c(0, 10), ylim = c(0, 5), axes = FALSE, xlab = "", ylab = "",
       main = "Transition diagram (3-state example)")
  x <- c(1.5, 5, 8.5); y <- 2.5
  for (i in 1:2) {
    arrows(x[i] + 0.6, y + 0.35, x[i + 1] - 0.6, y + 0.35, length = 0.12, lwd = 2, col = COL_ACCENT)
    text((x[i] + x[i + 1]) / 2, y + 0.7, sprintf("%.3f", P[i, i + 1]), cex = 0.9)
    arrows(x[i + 1] - 0.6, y - 0.35, x[i] + 0.6, y - 0.35, length = 0.12, lwd = 2, col = COL_ACCENT)
    text((x[i] + x[i + 1]) / 2, y - 0.7, sprintf("%.3f", P[i + 1, i]), cex = 0.9)
  }
  symbols(x, rep(y, 3), circles = rep(0.6, 3), inches = FALSE, add = TRUE,
          bg = "white", fg = COL_ACCENT, lwd = 3)
  text(x, y, c("0", "1", "2"), cex = 1.6, font = 2)
  for (i in 1:3) text(x[i], y - 1.3, sprintf("stay: %.3f", P[i, i]), cex = 0.85, col = COL_GREY)
}
fig_diagram_3states <- function(P, path) save_png(path, 1600, 800, 200, function() draw_diagram_3states(P))

# ---------- 2. Heat map of P ----------
draw_heatmap_P <- function(P, title) {
  op <- par(mar = c(4, 4, 3, 2)); on.exit(par(op))
  n <- nrow(P)
  image(1:n, 1:n, t(P[n:1, ]), col = colorRampPalette(c("white", COL_ACCENT))(100),
        xlab = "index of the destination state j", ylab = "index of the origin state i (top: i = 1)",
        main = title, axes = FALSE)
  pos <- seq(1, n, by = 4)
  axis(1, at = pos, labels = pos)
  axis(2, at = n + 1 - pos, labels = pos)
  box()
}
fig_heatmap_P <- function(P, path, title) save_png(path, 1400, 1300, 180, function() draw_heatmap_P(P, title))

# ---------- 3. Convergence to the stationary distribution ----------
convergence_distances <- function(P, pi_vec, nmax = 150) {
  v <- c(1, rep(0, nrow(P) - 1))               # start: empty bank
  d <- numeric(nmax + 1); d[1] <- 0.5 * sum(abs(v - pi_vec))
  for (k in 1:nmax) { v <- as.numeric(v %*% P); d[k + 1] <- 0.5 * sum(abs(v - pi_vec)) }
  d
}
draw_convergence <- function(P, pi_vec, title, nmax = 150) {
  d <- convergence_distances(P, pi_vec, nmax)
  plot(0:nmax, d, type = "l", lwd = 3, col = COL_ACCENT, log = "y",
       xlab = "Number of minutes n", ylab = "Distance to π (total variation, log scale)",
       main = title)
  grid()
}
fig_convergence <- function(P, pi_vec, path, title, nmax = 150) {
  save_png(path, 1400, 900, 200, function() draw_convergence(P, pi_vec, title, nmax))
  invisible(convergence_distances(P, pi_vec, nmax))
}

# ---------- 4. Scenario comparison ----------
draw_scenarios <- function(df) {
  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
  ymax <- max(df$Wq_ordinary, df$Wq_priority, na.rm = TRUE) * 1.15
  for (per in unique(df$period)) {
    d <- df[df$period == per, ]
    m <- rbind(d$Wq_priority, d$Wq_ordinary); colnames(m) <- d$tellers
    barplot(m, beside = TRUE, col = c(COL_2, COL_ACCENT), ylab = "Mean waiting time in queue (min)",
            xlab = "Number of tellers", main = paste("Hours:", gsub("_", "-", per)), ylim = c(0, ymax))
    if (per == unique(df$period)[1])
      legend("topright", legend = c("Priority", "Ordinary"), fill = c(COL_2, COL_ACCENT), bty = "n")
  }
}
fig_scenarios <- function(df, path) save_png(path, 1800, 900, 200, function() draw_scenarios(df))

# ---------- 5. Validation: theory versus simulation ----------
draw_validation <- function(pi_theory, pi_sim, title) {
  op <- par(mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
  barplot(rbind(pi_theory, pi_sim), beside = TRUE, col = c(COL_ACCENT, COL_2),
          xlab = "State index (a, b)", ylab = "Probability", main = title,
          names.arg = seq_along(pi_theory), cex.names = 0.6)
  legend("topright", legend = c("Computation (π)", "Monte Carlo simulation"),
         fill = c(COL_ACCENT, COL_2), bty = "n")
}
fig_validation <- function(pi_theory, pi_sim, path, title) save_png(path, 1800, 900, 200, function() draw_validation(pi_theory, pi_sim, title))

# Runs everything: computations, simulation, figures, tests.  Usage: Rscript run_all.R
args <- commandArgs(trailingOnly = FALSE)
f <- sub("--file=", "", args[grep("--file=", args)])
if (length(f)) {
  setwd(dirname(normalizePath(f)))                      # launched with Rscript
} else {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) setwd(dirname(normalizePath(of)))   # RStudio: Source button
}
if (!file.exists("data/parameters.json"))
  stop("Wrong working directory: open the project 'projet-markov.Rproj' or use setwd() to go to the project folder.")
for (s in c("model", "simulation", "scenarios", "figures")) source(file.path("src", paste0(s, ".R")), encoding = "UTF-8")
dir.create("figures", showWarnings = FALSE); dir.create("outputs", showWarnings = FALSE)
params <- load_parameters()

# --- 1. 3-state example (hand computation of the report) ---
ex <- params$hand_example
P3 <- build_P_simple(ex$K, ex$lambda, ex$s)
pi3 <- stationary_distribution(P3)
P3_2 <- P3 %*% P3
fig_diagram_3states(P3, "figures/fig1_diagram_3states.png")

# --- 2. Scenarios ---
sc <- run_scenarios(params)
write.csv(sc, "outputs/scenarios.csv", row.names = FALSE)
fig_scenarios(sc, "figures/fig4_scenarios.png")

# --- 3. Reference scenario: peak, 2 tellers ---
lp <- params$periods$peak$lambda_priority; lo <- params$periods$peak$lambda_ordinary; c_ref <- 2
P <- build_P(params$K, c_ref, params$s, lp, lo)
pv <- stationary_distribution(P)
fig_heatmap_P(P, "figures/fig2_heatmap_P.png",
              sprintf("Matrix P (peak, %d tellers, %d states)", c_ref, nrow(P)))
fig_convergence(P, pv, "figures/fig3_convergence.png", "Convergence to the stationary distribution")

# --- 4. Monte Carlo validation ---
sm <- params$simulation
sim <- simulate_bank(params$K, c_ref, params$s, lp, lo, sm$steps, sm$seed, sm$warmup)
pis <- frequencies_in_order(sim, attr(P, "states"))
fig_validation(pv, pis, "figures/fig5_validation.png", "Theory vs simulation (peak, 2 tellers)")
ind <- compute_indicators(P, pv, params$K, c_ref, params$s, lp, lo)

needed <- required_tellers(sc, params$wait_threshold_min)
results <- list(
  wait_threshold_min = params$wait_threshold_min,
  required_tellers = as.list(needed),
  example3 = list(P = P3, P2 = P3_2, pi = pi3),
  reference = list(period = "peak", tellers = c_ref, n_states = nrow(P),
                   indicators = ind,
                   max_pi_gap = max(abs(pis - pv)),
                   mean_pi_gap = mean(abs(pis - pv)),
                   simulation = list(steps = sim$n_steps, throughput_p = sim$throughput_p, throughput_o = sim$throughput_o,
                                     rejected_p = sim$rejected_p, rejected_o = sim$rejected_o)),
  scenarios = sc)
jsonlite::write_json(results, "outputs/results.json", pretty = TRUE, digits = 8, auto_unbox = TRUE)

cat("== 3-state example ==\n"); print(round(P3, 4)); print(round(pi3, 4))
cat("\n== Scenarios ==\n"); print(format(sc, digits = 3), row.names = FALSE)
cat(sprintf("\nValidation (peak, 2 tellers): max gap |pi_sim - pi| = %.4f\n", max(abs(pis - pv))))

# --- 5. Display the plots in the Plots tab (RStudio) ---
# The same plots are already saved as PNG files in the 'figures' folder.
if (interactive()) {
  cat("\nDisplaying the 5 plots in the Plots tab (use the arrows ◀ ▶ to move between them)...\n")
  show_plot(function() draw_diagram_3states(P3))
  show_plot(function() draw_heatmap_P(P, sprintf("Matrix P (peak, %d tellers, %d states)", c_ref, nrow(P))))
  show_plot(function() draw_convergence(P, pv, "Convergence to the stationary distribution"))
  show_plot(function() draw_scenarios(sc))
  show_plot(function() draw_validation(pv, pis, "Theory vs simulation (peak, 2 tellers)"))
}

# --- 6. Tests ---
if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
}

cat("\nThe 5 images are also saved in the 'figures' folder (Files tab).\n")
if (interactive()) {
  cat("To watch the bank minute by minute: open demo_simulation.R then click Source.\n")
} else {
  cat("(The plots appear in the Plots tab when this script is run from RStudio.)\n")
}

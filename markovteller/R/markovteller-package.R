#' markovteller: Markov chains for a bank queue
#'
#' The package models a bank with several tellers and priority customers as a
#' discrete-time Markov chain (one step = 1 minute). It builds the transition
#' matrix ([build_P()]), computes the stationary distribution
#' ([stationary_distribution()]) and waiting-time indicators
#' ([compute_indicators()]), compares scenarios ([run_scenarios()]) and
#' validates everything with a Monte Carlo simulation ([simulate_bank()]).
#' The parameters shipped with the package are **fictitious**.
#'
#' @keywords internal
"_PACKAGE"

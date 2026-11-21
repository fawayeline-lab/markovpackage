#' Load the (fictitious) parameters of the project
#'
#' Reads a JSON file. By default, reads the file shipped with the package,
#' whose values are **fictitious**.
#'
#' @param path Path to a JSON parameter file.
#' @return A list (K, s, tellers, periods, hand_example, simulation).
#' @examples
#' params <- load_parameters()
#' params$K
#' @export
load_parameters <- function(path = system.file("extdata", "parameters.json",
                                               package = "markovteller")) {
  if (!nzchar(path) || !file.exists(path)) stop("Parameter file not found.")
  jsonlite::fromJSON(path)
}

#' Scenarios: number of tellers x period (off-peak / peak)
#'
#' For each combination, builds the matrix, computes the stationary
#' distribution and the performance indicators.
#'
#' @param params List of parameters (see [load_parameters()]).
#' @return A data.frame: `period`, `tellers`, `load`, `L`, `Wq_priority`,
#'   `Wq_ordinary`, `W_priority`, `W_ordinary`, `p_full`, `occupancy`. Waiting
#'   times are in minutes.
#' @examples
#' run_scenarios(load_parameters())
#' @export
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
        load = (lp + lo) / (c * params$s),
        L = ind$L, Wq_priority = ind$Wq_p, Wq_ordinary = ind$Wq_o,
        W_priority = ind$W_p, W_ordinary = ind$W_o,
        p_full = ind$p_full, occupancy = ind$occupancy)
    }
  }
  do.call(rbind, rows)
}

#' Number of tellers needed for an acceptable waiting time
#'
#' Smallest number of tellers such that the mean waiting time of ordinary
#' customers does not exceed `threshold` minutes, for each period.
#'
#' @param sc Result of [run_scenarios()].
#' @param threshold Maximum accepted waiting time, in minutes.
#' @return A named vector (`off_peak`, `peak`); `NA` if none of the tested
#'   numbers of tellers is enough.
#' @examples
#' required_tellers(run_scenarios(load_parameters()), threshold = 1)
#' @export
required_tellers <- function(sc, threshold) {
  sapply(split(sc, sc$period), function(d) {
    ok <- d$tellers[d$Wq_ordinary <= threshold]
    if (length(ok)) min(ok) else NA_integer_
  })
}

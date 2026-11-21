# markovteller

R package: **Markov chains applied to a bank queue** (several tellers, priority customers). Bachelor year 3 project.

> The parameters shipped with the package (`inst/extdata/parameters.json`) are **fictitious**.

## Dependencies
- `jsonlite` (imported), `stats` (base R)
- `testthat` (suggested, for the tests)
- R >= 4.0.0

## Local installation

From R, from the folder that contains `markovteller`:

```r
install.packages("jsonlite")
install.packages("markovteller", repos = NULL, type = "source")
```

With `devtools`:

```r
devtools::install_local("markovteller")
```

## Example

```r
library(markovteller)

# 3-state example (one customer class, 1 teller, capacity 2)
P3 <- build_P_simple(K = 2, lambda = 0.3, s = 0.4)
stationary_distribution(P3)          # (56, 60, 27) / 143

# Full case: peak time, 2 tellers
params <- load_parameters()
P <- build_P(params$K, c = 2, params$s, lp = 0.10, lo = 0.45)
pv <- stationary_distribution(P)
compute_indicators(P, pv, params$K, c = 2, params$s, lp = 0.10, lo = 0.45)$Wq_o   # waiting time of ordinary customers (min)

# Compare 1 to 5 tellers, off-peak and peak
run_scenarios(params)

# Monte Carlo validation
sim <- simulate_bank(params$K, 2, params$s, 0.10, 0.45, steps = 100000)
```

## Functions

| Function | Role |
|---|---|
| `build_states()` | List of states (a, b) |
| `build_P()` | Transition matrix of the full model |
| `build_P_simple()` | Matrix of the one-class, one-teller example |
| `stationary_distribution()` | Solves πP = π |
| `stationary_eigenvector()` | Same result by eigenvector (check) |
| `distribution_after_n()` | π(n) = π(0)·Pⁿ |
| `compute_indicators()` | Probability of a full bank, mean number of customers, waiting times (Little's law), occupancy |
| `run_scenarios()` | 1 to 5 tellers × off-peak / peak |
| `required_tellers()` | Smallest number of tellers under a waiting threshold |
| `bank_trajectory()` | Number of customers minute by minute (for a plot) |
| `simulate_bank()` | Monte Carlo simulation independent of the matrix |
| `load_parameters()` | Reads the JSON parameters |

## Development

Run from RStudio, in the project `markovteller.Rproj`:

```r
devtools::document()   # regenerates documentation and NAMESPACE
devtools::test()       # runs the testthat tests
devtools::check()      # full check (R CMD check)
```

## License

MIT, see `LICENSE.md`.

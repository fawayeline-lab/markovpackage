# The model: bank tellers with priority customers

> **Fictitious data.** All numerical parameters (`data/parameters.json`) are made up to illustrate the model. They come from no real bank. For a real study, they would have to be estimated by timing arrivals and services.

## 1. Real-life problem

Customers of a branch wait too long at peak time. Question: how many tellers (from 1 to 5) should be opened so that waiting stays reasonable, knowing that some customers have priority?

## 2. Assumptions (stated explicitly)

1. Time is divided into steps of **1 minute**.
2. **State**: (a, b), with a priority customers and b ordinary customers in the bank (being served or waiting). The bank accepts at most **K = 8** customers: a + b ≤ K.
3. **Arrivals**: at each minute, at most one customer arrives. They are priority with probability λₚ, ordinary with probability λₒ (λₚ + λₒ ≤ 1). If they arrive when a + b = K at the start of the minute, they are refused (lost customer).
4. **Services**: there are c tellers. The number of busy tellers is m = min(a + b, c). At each minute, **each busy teller finishes its service with probability s, independently of the others**. Each service therefore lasts 1/s minutes on average (geometric law, memoryless).
5. **Priority**: priority customers are served before ordinary ones: they occupy min(a, c) tellers, ordinary customers take the remaining ones.
6. Arrival and departure within the same step are independent. Parameters do not change within a given period (off-peak or peak), so the chain is homogeneous.

## 3. Transition matrix

From state (a, b), we combine 3 arrival outcomes (none, priority, ordinary) and 3 departure outcomes (none, priority, ordinary). Each combination has probability equal to the product of the two probabilities and leads to a neighbouring state (a ± 1, b ± 1).

**Proof that each row sums to 1.** The three arrival probabilities are (1 − λₚ − λₒ, λₚ, λₒ), which sum to 1 (or (1, 0, 0) if the bank is full). The number of services completed among the tellers serving priority customers (then ordinary ones) follows a binomial law with parameter s: its probabilities also sum to 1. The sum of all the products is therefore 1 × 1 × 1 = 1. This property is also checked numerically by a test (`tests/testthat/test_model.R`).

With K = 8, the number of states is (K + 1)(K + 2)/2 = 45 (it can be read in `outputs/resultats.json`).

## 4. A 3-state example, to do by hand

One class, one teller, K = 2, λ = 0.3 and s = 0.4 (parameters `exemple_main`). States: 0, 1, 2 customers. The transition probabilities are:

- from 0: arrival (0.3), otherwise nothing (0.7);
- from 1: arrival without departure (0.3 × 0.6 = 0.18), departure without arrival (0.4 × 0.7 = 0.28), otherwise nothing (0.54);
- from 2 (full, arrival refused): departure (0.4), otherwise nothing (0.6).

Hence the matrix (the one in `outputs/resultats.json`):

```
        0     1     2
  0   0.70  0.30  0.00
  1   0.28  0.54  0.18
  2   0.00  0.40  0.60
```

**Stationary distribution.** πP = π gives, by isolating the flows between neighbouring states: 0.30·π₀ = 0.28·π₁ and 0.18·π₁ = 0.40·π₂. So π₁ = (15/14)·π₀ and π₂ = (9/20)·π₁ = (27/56)·π₀. With π₀ + π₁ + π₂ = 1: π₀·(56 + 60 + 27)/56 = 1, i.e. π₀ = 56/143 ≈ 0.3916, π₁ = 60/143 ≈ 0.4196 and π₂ = 27/143 ≈ 0.1888. The code finds exactly these values.

*Reading*: in the long run, the bank is empty 39% of the time and full 19% of the time.

## 5. Computed indicators

For each scenario, from π:

- probability that the bank is full: Σ π(a, b) over the states with a + b = K;
- effective input rate of each class: λ_eff = λ × (1 − P(full));
- mean number of customers in the bank (L) and in the queue (Lq);
- **Little's law** [S2]: W = L / λ_eff is the mean time in the bank, Wq = Lq / λ_eff the mean waiting time before reaching a teller;
- occupancy rate = E[m] / c.

## 6. Validation by Monte Carlo simulation

`src/simulation.R` simulates the bank minute by minute **without using the matrix P**: it draws at random the arrival and the end of service of each busy teller. If the observed frequencies fall back on π, the matrix P is correct. The seed is fixed (123) so that results are reproducible. A test also checks that the simulated mean number of customers satisfies Little's law.

## 6 bis. A day simulated customer by customer

`src/day.R` (run by `demo_simulation.R`) plays a day from 8:00 to 16:00, with the peak from 11:00 to 14:00. Each customer is tracked individually: arrival time, start of service, waiting time. The rules are those of the model (at most one arrival per minute, refusal if the bank already holds K customers, each busy teller finishes with probability s, priority customers first).

Two modes exist for priority customers:

- **without interruption** (default, realistic): a service already started is never interrupted. This is the mode of the simulated day.
- **with interruption**: a priority customer can take the place of an ordinary one being served (the ordinary one goes back to the queue). This is the implicit assumption of the matrix P. In this mode, the simulation matches the calculation (simulated waits of 30.3 / 7.7 / 1.15 / 0.16 / 0.02 minutes for ordinary customers, against 31.1 / 8.0 / 1.22 / 0.18 / 0.02 by calculation, at peak time with 1 to 5 tellers).

Waiting times of ordinary customers barely depend on this choice; those of priority customers do.

## 7. Limits of the model (honest)

- At most one arrival per minute: a simplification of continuous time.
- Geometric (memoryless) service times, whereas real services are less spread out.
- No impatient customers leaving the queue, no switching queues.
- Arrivals independent from one minute to the next, and constant parameters within a period.
- Fictitious data. The conclusions show the method, not the reality of a specific bank.

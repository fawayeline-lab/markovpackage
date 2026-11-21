# Research notes: Markov chains and their transition matrix

Level: Bachelor year 3 (L3 SDIA). Definitions are reworded from the sources listed in `sources.md`. Notation used throughout the project: states 0, 1, …, K; transition matrix P = (pᵢⱼ); distribution at time n written π(n), as a row vector; stationary distribution written π.

## 1. Markov chain

A **stochastic process** is a sequence of random variables X₀, X₁, X₂, … describing how a system evolves over time. Here, Xₙ is the **state** of the system at time n.

It is a (discrete-time) **Markov chain** if the future depends on the past only through the present:

P(Xₙ₊₁ = j | X₀ = i₀, …, Xₙ = i) = P(Xₙ₊₁ = j | Xₙ = i)

The chain is said to be "memoryless" [S1]. If this probability does not depend on n, the chain is **homogeneous**: this is the case studied here.

## 2. Transition matrix

Let pᵢⱼ = P(Xₙ₊₁ = j | Xₙ = i), the probability of going from state i to state j in one step. The matrix P = (pᵢⱼ) is a **stochastic matrix** [S1]:

- all its entries are ≥ 0 (they are probabilities);
- each row sums to 1 (from state i, the system necessarily goes somewhere).

**Reading a row**: row i gives the distribution of the next state, given that we are in state i. A column has no imposed sum.

## 3. Distribution after n steps

If π(0) is the distribution of the initial state (row vector), the distribution at time n is [S1]:

π(n) = π(0) · Pⁿ

Entry (i, j) of Pⁿ is the probability of going from i to j in exactly n steps.

## 4. Stationary distribution

A row vector π with entries ≥ 0 summing to 1 is **stationary** if [S1]:

π · P = π

If we start with distribution π, we still have distribution π at every later time. In other words, π is a left eigenvector of P for eigenvalue 1.

**Existence and uniqueness.** A chain is **irreducible** if every state can be reached from every other. A finite irreducible chain has a unique stationary distribution [S1]. If it is also **aperiodic** (no forced cycle), Pⁿ converges to a matrix whose rows are all equal to π: we get there from any starting state [S1].

**Computation.** Two equivalent methods:
1. solve the linear system π(P − I) = 0 with Σπᵢ = 1;
2. find the left eigenvector for eigenvalue 1, then normalise it.

## 5. Classification of states (briefly)

- **Recurrent**: we are certain to come back. **Transient**: there is a non-zero probability of never coming back [S1].
- **Absorbing**: a state that cannot be left (pᵢᵢ = 1) [S1].
- **Periodic**: returns only happen at multiples of an integer k > 1. **Aperiodic** if k = 1 [S1].

## 6. Where engineers use it

- **Queues**: Markov chains are the basis of the analytical treatment of queues [S1], [S3].
- **PageRank (Google)**: the ranking of web pages is defined by the stationary distribution of a Markov chain over pages, with a damping factor [S1], [S4].
- **Finance and economics**: modelling income distribution, firm size and asset prices; rating agencies publish tables of transition probabilities between credit ratings [S1].
- **Reliability and computing**: reference book on the links between reliability, queues and Markov chains [R4]. *I did not read its content, so I claim nothing more specific.*
- **Other fields mentioned**: biology, chemistry, physics, signal and speech processing, Bayesian statistics [S1].

## 7. Choice of field: operations research and queues

Queueing theory is the mathematical study of waiting lines [S3]. It is generally considered a branch of operations research: its results help decide which resources a service should provide [S3]. It computes the mean number of customers, the mean waiting time and the utilisation rate of the servers, then compares alternatives [S3].

**Relevance for a bank** (the project's own reasoning, not a claim from the sources): a branch must choose how many tellers to open without making customers wait too long or leaving tellers idle. This is exactly the sizing question this field deals with. Customers who arrive at random and a service of variable length are naturally modelled by a Markov chain whose state is the number of customers present.

**History**: Agner Krarup Erlang, a Danish engineer at the Copenhagen telephone exchange, published the first paper on queues in 1909 [S3], [R2].

**Kendall notation**: a simple queue is written A/S/c (arrival law / service law / number of servers). M/M/1: Poisson arrivals, exponential service times, 1 server [S3].

## 8. Little's law

L = λ · W [S2], where L is the mean number of customers in the system, λ the mean **effective** arrival rate (customers actually admitted) and W the mean time spent in the system. It is enough for the system to be stationary and ergodic; no assumption on the arrival or service laws is needed [S2]. Original reference: Little (1961) [R1].

## 9. Glossary

| Term | Short definition |
|---|---|
| State | Situation of the system at a given time (here: number of customers) |
| Stochastic matrix | Entries ≥ 0, each row sums to 1 |
| Markov property | The future depends only on the present |
| Homogeneous | Transition probabilities do not change over time |
| Irreducible | Every state is reachable from every other |
| Aperiodic | No forced cycle in the returns |
| Stationary distribution | π such that πP = π |
| Ergodic | Irreducible, aperiodic and positive recurrent |
| Absorbing | A state that cannot be left |
| Occupancy rate | Share of time a teller is in use |
| Capacity K | Maximum number of customers admitted to the system |
| Monte Carlo | Estimating by repeated random simulation |

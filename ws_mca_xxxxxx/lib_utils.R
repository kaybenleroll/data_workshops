# Utility functions for MCA workshop

# Resolve package conflicts
resolve_conflicts <- function(packages) {
  for (pkg in packages) {
    conflicted::conflict_prefer_all(pkg, quiet = TRUE)
  }
  invisible(NULL)
}

# Shannon entropy calculation
shannon_entropy <- function(probs) {
  # Remove zero probabilities (0 * log(0) = 0 by convention)
  probs <- probs[probs > 0]
  -sum(probs * log2(probs))
}

# Entropy from counts
entropy_from_counts <- function(counts) {
  probs <- counts / sum(counts)
  shannon_entropy(probs)
}

# Simulate Poisson arrivals
simulate_poisson_arrivals <- function(n, lambda) {
  cumsum(rexp(n, rate = lambda) / lambda)
}

# Calculate M/M/1 queue metrics
calculate_mm1_metrics <- function(lambda, mu) {
  rho <- lambda / mu
  L <- rho / (1 - rho)
  W <- 1 / (mu - lambda)
  Lq <- rho^2 / (1 - rho)
  Wq <- rho / (mu - lambda)
  
  list(
    rho = rho,
    L = L,
    W = W,
    Lq = Lq,
    Wq = Wq
  )
}

# Simulate entropy convergence
simulate_entropy_convergence <- function(probs, sample_sizes, n_reps = 100) {
  results <- expand_grid(
    rep = 1:n_reps,
    n = sample_sizes
  ) |>
    mutate(
      samples = map2(n, rep, function(n, seed) {
        set.seed(seed)
        sample(1:length(probs), size = n, replace = TRUE, prob = probs)
      }),
      entropy_estimate = map_dbl(samples, function(samp) {
        counts <- tabulate(samp, nbins = length(probs))
        entropy_from_counts(counts)
      })
    ) |>
    select(-samples)
  
  return(results)
}

# Write log entry (simple logging)
write_log_entry <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(glue("[{timestamp}] {level}: {message}\n"))
}

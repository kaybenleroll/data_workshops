# =============================================================================
# Queueing Theory Helper Functions
# =============================================================================
# This library provides functions for analytical queue calculations,
# discrete-event simulation, and visualization of queueing systems.
#
# Author: Mick Cooney <mickcooney@gmail.com>
# =============================================================================


# =============================================================================
# ANALYTICAL QUEUE CALCULATIONS
# =============================================================================

#' Calculate M/M/1 Queue Metrics
#'
#' Computes all standard performance metrics for an M/M/1 queue (single-server
#' queue with Poisson arrivals and exponential service times).
#'
#' @param lambda Numeric: Arrival rate (customers per time unit)
#' @param mu Numeric: Service rate (customers per time unit)
#'
#' @return Named list with metrics:
#'   - rho: Utilization (0 to 1)
#'   - L: Average number in system
#'   - Lq: Average number in queue
#'   - W: Average time in system
#'   - Wq: Average wait time in queue
#'   - P0: Probability of empty system
#'
#' @examples
#' metrics <- calculate_mm1_metrics(lambda = 4, mu = 5)
#' metrics$rho  # 0.8
#' metrics$L    # 4.0
#'
calculate_mm1_metrics <- function(lambda, mu) {

  # Validate inputs
  if (lambda <= 0 || mu <= 0) {
    stop("lambda and mu must be positive")
    }

  if (lambda >= mu) {
    stop("System unstable: lambda must be less than mu (utilization < 1)")
    }

  # Calculate utilization
  rho <- lambda / mu

  # Calculate queue metrics
  P0 <- 1 - rho
  L <- rho / (1 - rho)
  Lq <- rho^2 / (1 - rho)
  W <- 1 / (mu - lambda)
  Wq <- rho / (mu - lambda)

  return(
    list(
      rho = rho,
      L = L,
      Lq = Lq,
      W = W,
      Wq = Wq,
      P0 = P0
      )
    )
}


#' Calculate M/M/c Queue Metrics
#'
#' Computes performance metrics for an M/M/c multi-server queue using the
#' Erlang C formula.
#'
#' @param lambda Numeric: Arrival rate (customers per time unit)
#' @param mu Numeric: Service rate per server (customers per time unit)
#' @param c Integer: Number of servers
#'
#' @return Named list with metrics:
#'   - c: Number of servers
#'   - rho: Per-server utilization (0 to 1)
#'   - L: Average number in system
#'   - Lq: Average number in queue
#'   - W: Average time in system
#'   - Wq: Average wait time in queue
#'   - P0: Probability of empty system
#'   - Pw: Probability of waiting (Erlang C)
#'
#' @examples
#' metrics <- calculate_mmc_metrics(lambda = 20, mu = 6, c = 4)
#' metrics$rho  # Per-server utilization
#' metrics$Pw   # Probability customer waits
#'
calculate_mmc_metrics <- function(lambda, mu, c) {

  # Validate inputs
  if (lambda <= 0 || mu <= 0) {
    stop("lambda and mu must be positive")
    }

  if (c < 1 || c != as.integer(c)) {
    stop("c must be a positive integer")
    }

  if (lambda >= c * mu) {
    stop("System unstable: lambda must be less than c*mu")
    }

  # Calculate utilization
  rho <- lambda / (c * mu)
  a <- lambda / mu  # Offered load

  # Calculate P0 using summation formula
  sum_term <- sum(sapply(0:(c-1), function(n) a^n / factorial(n)))
  P0 <- 1 / (sum_term + (a^c / factorial(c)) * (1 / (1 - rho)))

  # Erlang C formula (probability of waiting)
  Pw <- (a^c / factorial(c)) * (1 / (1 - rho)) * P0

  # Calculate queue metrics
  Lq <- Pw * rho / (1 - rho)
  Wq <- Lq / lambda
  W <- Wq + 1/mu
  L <- lambda * W

  return(
    list(
      c = c,
      rho = rho,
      L = L,
      Lq = Lq,
      W = W,
      Wq = Wq,
      P0 = P0,
      Pw = Pw
      )
    )
}


#' Calculate M/D/1 Queue Metrics
#'
#' Computes performance metrics for M/D/1 queue with deterministic (constant)
#' service times.
#'
#' @param lambda Numeric: Arrival rate (customers per time unit)
#' @param mu Numeric: Service rate (1/service_time)
#'
#' @return Named list with metrics:
#'   - rho: Utilization
#'   - L: Average number in system
#'   - Lq: Average number in queue
#'   - W: Average time in system
#'   - Wq: Average wait time in queue
#'   - Cs: Coefficient of variation of service time (always 0 for M/D/1)
#'
#' @examples
#' metrics <- calculate_md1_metrics(lambda = 4, mu = 5)
#' metrics$Lq  # Exactly half of equivalent M/M/1
#'
calculate_md1_metrics <- function(lambda, mu) {

  # Validate inputs
  if (lambda <= 0 || mu <= 0) {
    stop("lambda and mu must be positive")
    }

  if (lambda >= mu) {
    stop("System unstable: lambda must be less than mu")
    }

  # Calculate utilization
  rho <- lambda / mu

  # M/D/1 formulas (deterministic service, Cs = 0)
  Lq <- rho^2 / (2 * (1 - rho))
  Wq <- Lq / lambda
  W <- Wq + 1/mu
  L <- lambda * W

  return(
    list(
      rho = rho,
      L = L,
      Lq = Lq,
      W = W,
      Wq = Wq,
      Cs = 0  # Deterministic service has zero variance
      )
    )
}


#' Calculate M/G/1 Queue Metrics (Pollaczek-Khinchine)
#'
#' Computes performance metrics for M/G/1 queue with general service time
#' distribution using the Pollaczek-Khinchine formula.
#'
#' @param lambda Numeric: Arrival rate (customers per time unit)
#' @param service_mean Numeric: Mean service time
#' @param service_var Numeric: Variance of service time
#'
#' @return Named list with metrics:
#'   - rho: Utilization
#'   - L: Average number in system
#'   - Lq: Average number in queue
#'   - W: Average time in system
#'   - Wq: Average wait time in queue
#'   - Cs: Coefficient of variation of service time
#'   - service_mean: Mean service time
#'   - service_sd: Standard deviation of service time
#'
#' @examples
#' metrics <- calculate_mg1_metrics(lambda = 4, service_mean = 0.2, service_var = 0.09)
#' metrics$Cs  # Coefficient of variation
#' metrics$Lq  # Depends on both mean and variance
#'
calculate_mg1_metrics <- function(lambda, service_mean, service_var) {

  # Validate inputs
  if (lambda <= 0 || service_mean <= 0 || service_var < 0) {
    stop("lambda and service_mean must be positive, service_var non-negative")
    }

  mu <- 1 / service_mean
  rho <- lambda * service_mean

  if (rho >= 1) {
    stop("System unstable: lambda * service_mean must be less than 1")
    }

  # Coefficient of variation
  service_sd <- sqrt(service_var)
  Cs <- service_sd / service_mean

  # Pollaczek-Khinchine formula
  Lq <- (lambda^2 * service_var + rho^2) / (2 * (1 - rho))
  Wq <- Lq / lambda
  W <- Wq + service_mean
  L <- lambda * W

  return(
    list(
      rho = rho,
      L = L,
      Lq = Lq,
      W = W,
      Wq = Wq,
      Cs = Cs,
      service_mean = service_mean,
      service_sd = service_sd
      )
    )
}


#' Calculate M/M/1/K Queue Metrics (Finite Capacity)
#'
#' Computes performance metrics for an M/M/1 queue with a maximum capacity K.
#' Customers arriving when the system is full are blocked (lost).
#'
#' @param lambda Numeric: Arrival rate
#' @param mu Numeric: Service rate
#' @param k Integer: Maximum capacity (including customer in service)
#'
#' @return Named list with metrics:
#'   - rho: Offered load (lambda/mu)
#'   - utilization: Actual server utilization
#'   - L: Average number in system
#'   - Lq: Average number in queue
#'   - W: Average time in system (for those who enter)
#'   - Wq: Average wait time (for those who enter)
#'   - Pk: Probability system is full (Blocking probability)
#'   - lambda_eff: Effective arrival rate
#'
calculate_mm1k_metrics <- function(lambda, mu, k) {
  if (lambda <= 0 || mu <= 0 || k < 1) stop("Inputs must be positive")

  rho <- lambda / mu

  # Calculate P0 and Pk
  if (rho == 1) {
    P0 <- 1 / (k + 1)
    Pk <- 1 / (k + 1)
    L  <- k / 2
  } else {
    P0 <- (1 - rho) / (1 - rho^(k + 1))
    Pk <- (rho^k) * P0
    L  <- (rho / (1 - rho)) - ((k + 1) * rho^(k + 1) / (1 - rho^(k + 1)))
  }

  lambda_eff <- lambda * (1 - Pk)
  W <- L / lambda_eff
  Wq <- W - (1 / mu)
  Lq <- lambda_eff * Wq
  utilization <- 1 - P0

  return(list(
    rho = rho,
    utilization = utilization,
    L = L,
    Lq = Lq,
    W = W,
    Wq = Wq,
    Pk = Pk,
    lambda_eff = lambda_eff
  ))
}


#' Calculate G/G/1 Queue Metrics (Kingman's Approximation)
#'
#' Uses Kingman's heavy traffic approximation to estimate wait times for
#' queues with non-Poisson arrivals and non-exponential service.
#'
#' @param lambda Numeric: Arrival rate
#' @param mu Numeric: Service rate
#' @param ca Numeric: Coefficient of variation of interarrival times
#' @param cs Numeric: Coefficient of variation of service times
#'
#' @return Estimated Wq (Average wait time in queue)
#'
calculate_gg1_approximation <- function(lambda, mu, ca, cs) {
  rho <- lambda / mu
  if (rho >= 1) stop("System unstable")

  # Kingman's formula
  Wq <- (rho / (mu * (1 - rho))) * ((ca^2 + cs^2) / 2)

  return(Wq)
}


# =============================================================================
# SIMULATION HELPER FUNCTIONS
# =============================================================================

#' Simulate Poisson Arrivals
#'
#' Generates arrival times from a Poisson process with specified rate. Uses
#' exponential interarrival times.
#'
#' @param n Integer: Number of arrivals to simulate
#' @param lambda Numeric: Arrival rate (arrivals per time unit)
#' @param start_time Numeric: Starting time for first possible arrival (default 0)
#'
#' @return Numeric vector of arrival times in ascending order
#'
#' @examples
#' arrivals <- simulate_poisson_arrivals(n = 100, lambda = 5)
#' mean(diff(arrivals))  # Should be approximately 1/5 = 0.2
#'
simulate_poisson_arrivals <- function(n, lambda, start_time = 0) {

  if (n < 1 || lambda <= 0) {
    stop("n must be positive integer, lambda must be positive")
    }

  # Generate interarrival times (exponential)
  interarrivals <- rexp(n, rate = lambda)

  # Cumulative sum gives arrival times
  arrivals <- start_time + cumsum(interarrivals)

  return(arrivals)
}


#' Create Basic simmer Trajectory for Queue
#'
#' Creates a standard customer trajectory: seize resource, delay for service,
#' release resource.
#'
#' @param resource_name Character: Name of the resource/server
#' @param service_dist Function: Function returning service time (called with no args)
#'
#' @return A simmer trajectory object
#'
#' @examples
#' # Exponential service with rate 5
#' traj <- create_queue_trajectory("server", function() rexp(1, 5))
#'
create_queue_trajectory <- function(resource_name, service_dist) {

  trajectory("customer") |>
    seize(resource_name, 1) |>
    timeout(service_dist) |>
    release(resource_name, 1)
}


#' Run simmer Queue Simulation and Extract Metrics
#'
#' Runs a queueing simulation and computes key performance metrics from the
#' monitoring data.
#'
#' @param sim_env A simmer environment object (already configured)
#' @param until Numeric: Simulation end time
#' @param warmup Numeric: Warmup period to discard from statistics (default 0)
#'
#' @return Named list with simulation metrics:
#'   - avg_wait: Average wait time in queue
#'   - avg_system_time: Average time in system
#'   - avg_queue_length: Time-averaged queue length
#'   - utilization: Server utilization
#'   - n_served: Number of customers served
#'   - arrivals_data: Tibble from get_mon_arrivals()
#'   - resources_data: Tibble from get_mon_resources()
#'
run_queue_simulation <- function(sim_env, until, warmup = 0) {

  # Run simulation
  run(sim_env, until = until)

  # Extract monitoring data
  arrivals <- get_mon_arrivals(sim_env) |>
    as_tibble() |>
    filter(end_time > warmup)  # Remove warmup period

  resources <- get_mon_resources(sim_env) |>
    as_tibble() |>
    filter(time > warmup)

  # Calculate metrics
  if (nrow(arrivals) > 0) {
    avg_wait <- mean(arrivals$wait_time, na.rm = TRUE)
    avg_system_time <- mean(arrivals$end_time - arrivals$start_time, na.rm = TRUE)
    n_served <- nrow(arrivals)
    } else {
    avg_wait <- NA
    avg_system_time <- NA
    n_served <- 0
    }

  if (nrow(resources) > 0) {
    # Time-weighted average queue length
    time_diffs <- c(diff(resources$time), 0)
    avg_queue_length <- sum(resources$queue * time_diffs) / sum(time_diffs)

    # Utilization (fraction of time server busy)
    utilization <- mean(resources$server / resources$capacity)
    } else {
    avg_queue_length <- NA
    utilization <- NA
    }

  return(
    list(
      avg_wait = avg_wait,
      avg_system_time = avg_system_time,
      avg_queue_length = avg_queue_length,
      utilization = utilization,
      n_served = n_served,
      arrivals_data = arrivals,
      resources_data = resources
      )
    )
}


#' Compare Analytical vs Simulated Results
#'
#' Runs multiple simulation replications and compares simulated statistics to
#' analytical results, computing confidence intervals.
#'
#' @param analytical_metrics Named list from calculate_* functions
#' @param simulation_func Function that runs one simulation and returns metrics list
#' @param n_reps Integer: Number of replications (default 30)
#' @param conf_level Numeric: Confidence level for intervals (default 0.95)
#'
#' @return Tibble comparing analytical values to simulated means with CI
#'
#' @examples
#' analytical <- calculate_mm1_metrics(lambda = 4, mu = 5)
#' sim_func <- function() {
#'   # ... simulation code ...
#'   return(list(Wq = ..., L = ...))
#' }
#' comparison <- compare_analytical_simulated(analytical, sim_func)
#'
compare_analytical_simulated <- function(
    analytical_metrics,
    simulation_func,
    n_reps = 30,
    conf_level = 0.95
) {

  # Run replications
  sim_results <- map(1:n_reps, ~simulation_func())

  # Extract common metrics
  metric_names <- c("L", "Lq", "W", "Wq", "rho")
  metric_names <- intersect(metric_names, names(analytical_metrics))

  # Compute statistics for each metric
  comparison_tbl <- map_dfr(metric_names, function(metric) {

    # Analytical value
    analytical_val <- analytical_metrics[[metric]]

    # Simulated values
    sim_vals <- map_dbl(sim_results, ~.x[[metric]])

    # Compute statistics
    sim_mean <- mean(sim_vals, na.rm = TRUE)
    sim_sd <- sd(sim_vals, na.rm = TRUE)
    sim_se <- sim_sd / sqrt(sum(!is.na(sim_vals)))

    # Confidence interval
    z_critical <- qnorm((1 + conf_level) / 2)
    ci_lower <- sim_mean - z_critical * sim_se
    ci_upper <- sim_mean + z_critical * sim_se

    # Percent error
    pct_error <- 100 * (sim_mean - analytical_val) / analytical_val

    tibble(
      metric = metric,
      analytical = analytical_val,
      simulated_mean = sim_mean,
      simulated_sd = sim_sd,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      pct_error = pct_error,
      within_ci = analytical_val >= ci_lower & analytical_val <= ci_upper
      )
    })

  return(comparison_tbl)
}


# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

#' Plot Queue Length Over Time
#'
#' Creates time series plot of queue length from simmer resource monitor data.
#'
#' @param resource_mon Tibble from get_mon_resources()
#' @param resource_name Character: Name of resource to plot
#'
#' @return ggplot object
#'
plot_queue_length <- function(resource_mon, resource_name) {

  plot_data <- resource_mon |>
    filter(resource == resource_name)

  ggplot(plot_data, aes(x = time, y = queue)) +
    geom_step(colour = "steelblue", linewidth = 0.8) +
    labs(
      title = glue("Queue Length Over Time: {resource_name}"),
      x = "Time",
      y = "Queue Length"
      ) +
    theme_minimal()
}


#' Plot Wait Time Distribution
#'
#' Creates histogram/density of customer wait times from simmer arrivals data.
#'
#' @param arrivals_mon Tibble from get_mon_arrivals()
#' @param theoretical_mean Optional numeric: theoretical mean for overlay
#'
#' @return ggplot object
#'
plot_wait_distribution <- function(arrivals_mon, theoretical_mean = NULL) {

  p <- ggplot(arrivals_mon, aes(x = wait_time)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 30,
      fill = "steelblue",
      alpha = 0.7
      ) +
    geom_density(colour = "firebrick", linewidth = 1) +
    labs(
      title = "Distribution of Wait Times",
      x = "Wait Time",
      y = "Density"
      )

  if (!is.null(theoretical_mean)) {
    p <- p + geom_vline(
      xintercept = theoretical_mean,
      linetype = "dashed",
      colour = "darkgreen",
      linewidth = 1
      ) +
      annotate(
        "text",
        x = theoretical_mean,
        y = Inf,
        label = glue("Theoretical mean: {round(theoretical_mean, 2)}"),
        vjust = 2,
        colour = "darkgreen"
        )
    }

  return(p)
}


#' Plot Utilization vs Performance
#'
#' Creates hockey-stick plot showing how metrics explode as utilization
#' approaches 1.
#'
#' @param metric Character: Which metric to plot ("L", "Lq", "W", "Wq")
#' @param queue_type Character: "MM1", "MD1", or "MG1"
#' @param rho_range Numeric vector: Range of utilization values
#' @param service_cv Numeric: For MG1, coefficient of variation (default 1.5)
#'
#' @return ggplot object
#'
plot_utilization_sensitivity <- function(
    metric = "L",
    queue_type = "MM1",
    rho_range = seq(0.1, 0.95, by = 0.05),
    service_cv = 1.5
) {

  # Calculate metric values across utilization range
  if (queue_type == "MM1") {
    values <- ifelse(metric == "L", rho_range / (1 - rho_range),
                    ifelse(metric == "Lq", rho_range^2 / (1 - rho_range),
                          NA))
    } else if (queue_type == "MD1") {
    values <- ifelse(metric == "Lq", rho_range^2 / (2 * (1 - rho_range)),
                    ifelse(metric == "L", rho_range^2 / (2 * (1 - rho_range)) + rho_range,
                          NA))
    } else if (queue_type == "MG1") {
    values <- ifelse(metric == "Lq",
                    rho_range^2 * (1 + service_cv^2) / (2 * (1 - rho_range)),
                    NA)
    }

  plot_data <- tibble(
    rho = rho_range,
    value = values
    )

  ggplot(plot_data, aes(x = rho, y = value)) +
    geom_line(colour = "firebrick", linewidth = 1.2) +
    geom_point(colour = "steelblue", size = 2) +
    labs(
      title = glue("{metric} vs Utilization ({queue_type} Queue)"),
      subtitle = "Hockey stick effect: metrics explode as ρ → 1",
      x = "Utilization (ρ)",
      y = metric
      ) +
    scale_x_continuous(labels = percent) +
    theme_minimal()
}


#' Plot Queue Comparison
#'
#' Side-by-side comparison of different queue types at same utilization.
#'
#' @param lambda Numeric: Arrival rate
#' @param mu Numeric: Service rate
#' @param queue_types Character vector: Types to compare (e.g., c("MM1", "MD1", "MG1"))
#' @param service_cv Numeric: Coefficient of variation for M/G/1 (default 1.5)
#'
#' @return ggplot object showing metric comparisons
#'
plot_queue_comparison <- function(
    lambda,
    mu,
    queue_types = c("MM1", "MD1", "MG1"),
    service_cv = 1.5
) {

  # Calculate metrics for each queue type
  results_lst <- list()

  if ("MM1" %in% queue_types) {
    mm1 <- calculate_mm1_metrics(lambda, mu)
    results_lst$MM1 <- tibble(
      queue_type = "M/M/1",
      L = mm1$L,
      Lq = mm1$Lq,
      W = mm1$W,
      Wq = mm1$Wq,
      rho = mm1$rho
      )
    }

  if ("MD1" %in% queue_types) {
    md1 <- calculate_md1_metrics(lambda, mu)
    results_lst$MD1 <- tibble(
      queue_type = "M/D/1",
      L = md1$L,
      Lq = md1$Lq,
      W = md1$W,
      Wq = md1$Wq,
      rho = md1$rho
      )
    }

  if ("MG1" %in% queue_types) {
    service_mean <- 1 / mu
    service_var <- (service_cv * service_mean)^2
    mg1 <- calculate_mg1_metrics(lambda, service_mean, service_var)
    results_lst$MG1 <- tibble(
      queue_type = glue("M/G/1 (Cv={service_cv})"),
      L = mg1$L,
      Lq = mg1$Lq,
      W = mg1$W,
      Wq = mg1$Wq,
      rho = mg1$rho
      )
    }

  # Combine results
  comparison_tbl <- bind_rows(results_lst)

  # Reshape for plotting
  plot_data <- comparison_tbl |>
    pivot_longer(
      cols = c(L, Lq, W, Wq),
      names_to = "metric",
      values_to = "value"
      )

  ggplot(plot_data, aes(x = queue_type, y = value, fill = queue_type)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    facet_wrap(~metric, scales = "free_y", ncol = 2) +
    labs(
      title = "Queue Type Comparison",
      subtitle = glue("λ = {lambda}, μ = {mu}, ρ = {round(lambda/mu, 2)}"),
      x = NULL,
      y = "Metric Value"
      ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
      )
}


#' Create simmer Trajectory with Reneging
#'
#' Customers leave the queue if they wait longer than their patience.
#'
#' @param resource_name Character: Resource name
#' @param service_dist Function: Service time distribution
#' @param patience_dist Function: Patience distribution
#'
#' @return simmer trajectory
#'
create_reneging_trajectory <- function(resource_name, service_dist, patience_dist) {
  trajectory("customer_renege") |>
    renege_in(patience_dist, out = trajectory("leave") |>
                        log_("Customer renounced!")) |>
    seize(resource_name, 1) |>
    renege_abort() |>
    timeout(service_dist) |>
    release(resource_name, 1)
}


#' Create simmer Trajectory with Priority
#'
#' Customers have different priority levels.
#'
#' @param resource_name Character: Resource name
#' @param service_dist Function: Service time distribution
#' @param priority_val Integer: Priority (higher is more priority)
#'
#' @return simmer trajectory
#'
create_priority_trajectory <- function(resource_name, service_dist, priority_val) {
  trajectory("customer_priority") |>
    set_prioritization(c(priority_val, 0, TRUE)) |>
    seize(resource_name, 1) |>
    timeout(service_dist) |>
    release(resource_name, 1)
}



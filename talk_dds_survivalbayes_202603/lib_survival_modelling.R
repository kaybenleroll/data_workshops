#' Determine policy status at a given date
#'
#' This function takes a tibble of policy data and a reference date,
#' then determines the status of each policy at that date based on:
#' - policy_startdate: when the policy became active
#' - policy_enddate: when the policy term ends
#' - policy_status: current status (e.g., 'lapsed', 'active', etc.)
#' - policy_statuschangedate: when the status changed
#'
#' @param policy_data A tibble containing policy information with columns:
#'   - policy_startdate (Date): Policy start date
#'   - policy_enddate (Date): Policy end date
#'   - policy_status (character): Current policy status
#'   - policy_statuschangedate (Date): Date when status changed
#'   - dob_life1 (Date): Date of birth of primary policyholder
#' @param reference_date A Date object representing the date at which to check status
#'
#' @return A tibble with additional columns:
#'   - status_at_reference_date: 'not_started', 'inforce', 'completed', 'lapsed', or 'unknown'
#'   - weeks_to_refdate: Weeks from policy start to reference date
#'   - weeks_to_statusdate: Weeks from policy start to status change date
#'   - policy_lifetime: Weeks of policy life (to reference date if inforce, to status date if lapsed)
#'   - age_at_refdate: Age of policyholder at reference date (in years)
#'   - age_at_statusdate: Age of policyholder at status change date (in years)
#'   - age_at_observation: Age at appropriate date based on policy status
#'
determine_policy_status <- function(policy_data_tbl, reference_date) {

  # Validate inputs
  if (!is.data.frame(policy_data_tbl)) {
    stop("policy_data must be a data frame or tibble")
    }

  if (!inherits(reference_date, "Date")) {
    stop("reference_date must be a Date object")
    }

  required_cols <- c(
    "policy_startdate",
    "policy_enddate",
    "policy_status",
    "policy_statuschangedate",
    "dob_life1"
    )

  missing_cols <- setdiff(required_cols, names(policy_data_tbl))

  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }

  # Ensure date columns are Date objects
  policy_data_tbl <- policy_data_tbl |>
    mutate(
      across(
        c(policy_startdate, policy_enddate, policy_statuschangedate, dob_life1),
        as.Date
        )
      )

  # Determine status at reference date
  updated_tbl <- policy_data_tbl |>
    filter(policy_startdate < reference_date) |>
    mutate(
      status_at_reference_date = case_when(
        # Policy hasn't started yet
        (reference_date < policy_startdate) ~
          "not_started",

        # Policy has expired (term ended)
        (reference_date > policy_enddate) ~
          "completed",

        # Policy has lapsed and the lapse occurred before or on the reference date
        ((policy_status == "lapsed") & (policy_statuschangedate <= reference_date)) ~
          "lapsed",

        # Policy is within term and either not lapsed or lapsed after reference date
        ((reference_date >= policy_startdate) & (reference_date <= policy_enddate)) ~
          "inforce",

        # Default case (shouldn't occur with proper data)
        TRUE ~ "unknown"
        ),

        weeks_to_refdate    = difftime(
          reference_date,          policy_startdate, units = "weeks"
          ) |> as.numeric(),
        weeks_to_statusdate = difftime(
          policy_statuschangedate, policy_startdate, units = "weeks"
          ) |> as.numeric(),

        policy_lifetime = if_else(
          policy_status == "inforce",
          weeks_to_refdate,
          weeks_to_statusdate
          ),

        # Calculate age at reference date
        age_at_refdate = time_length(
          difftime(reference_date, dob_life1),
          unit = "years"
          ),

        # Calculate age at status change date (for lapsed policies)
        age_at_statusdate = time_length(
          difftime(policy_statuschangedate, dob_life1),
          unit = "years"
          ),

        # Use appropriate age based on policy status
        age_at_observation = if_else(
          policy_status == "inforce",
          age_at_refdate,
          age_at_statusdate
          )
      )

  return(updated_tbl)
}


#' Extract Baseline Hazard from rstanarm stan_surv Fitted Object
#'
#' Extracts the baseline hazard function from a Cox proportional hazards model
#' fitted using rstanarm::stan_surv(). The function evaluates the M-spline
#' basis at specified time points and computes the baseline hazard for each
#' posterior draw. Follows the same computational approach as 
#' plot(stanfit, plotfun = "basehaz").
#'
#' For Cox-PH models, the baseline hazard represents the hazard when all
#' covariates are at their reference values. The function uses the M-spline
#' coefficients and basis stored in the fitted model to compute 
#' h_0(t) = basis(t) %*% coefficients.
#'
#' @param stanfit A stansurv object from rstanarm::stan_surv()
#' @param summary Logical: return summary statistics (TRUE) or all draws (FALSE)?
#'   Default FALSE returns all posterior draws
#' @param n Integer: number of time points to evaluate (default 1000)
#'
#' @return Tibble with baseline hazard estimates:
#'   If summary = FALSE: time, draw_id, baseline_hazard
#'   If summary = TRUE: time, mean_hazard, median_hazard, lower_ci, upper_ci
#'
#' @examples
#' # Extract all posterior draws
#' baseline_hazard_tbl <- extract_stansurv_baseline_hazard(lapse1_coxph_stansurv)
#' 
#' # Get summary statistics with 95% credible intervals
#' hazard_summary_tbl <- extract_stansurv_baseline_hazard(
#'   lapse1_coxph_stansurv, 
#'   summary = TRUE,
#'   n = 500
#' )
extract_stansurv_baseline_hazard <- function(stanfit, summary = FALSE, n = 1000) {

  # Validate the object
  if (!inherits(stanfit, "stansurv")) {
    stop("This function requires a 'stansurv' object from stan_surv()")
  }

  # Get time range from the data
  # Use the same sequence generation as plot.stansurv
  t_min <- min(stanfit$entrytime)
  t_max <- max(stanfit$eventtime)
  times <- seq(t_min, t_max, by = (t_max - t_min) / n)
  
  # Extract baseline hazard structure and parameters
  # Following the pattern from plot.stansurv in plots.R
  basehaz <- stanfit$basehaz
  
  # Extract parameter draws
  stanmat <- as.matrix(stanfit$stanfit)
  
  # Get aux parameters (these contain the M-spline coefficients)
  nms_aux <- colnames(stanmat)[grep("^m-splines-coef", colnames(stanmat))]
  if (length(nms_aux) == 0) {
    stop("Cannot find baseline hazard spline coefficients (m-splines-coef) in Stan output.")
  }
  aux <- stanmat[, nms_aux, drop = FALSE]
  
  # Get intercept (alpha) if present
  nms_int <- grep("^\\(Intercept\\)$", colnames(stanmat), value = TRUE)
  if (length(nms_int) > 0) {
    intercept <- stanmat[, nms_int, drop = FALSE]
  } else {
    intercept <- NULL
  }
  
  # Evaluate baseline hazard at the specified times
  # This uses the same internal logic as plot(stanfit, plotfun = "basehaz")
  # The formula is: basehaz = exp(intercept) * (aux %*% basis(times))
  # For Cox models without intercept: basehaz = aux %*% basis(times)
  
  # Get the M-spline basis matrix at the specified times
  basis_mat <- predict(basehaz$basis, times)
  
  # Compute baseline hazard for each draw
  n_draws <- nrow(aux)
  n_times <- length(times)
  
  # Matrix multiplication: (n_draws x n_coefs) %*% (n_coefs x n_times)
  hazard_matrix <- aux %*% t(basis_mat)
  
  if (!is.null(intercept)) {
    # With intercept: exp(intercept) * (aux %*% basis)
    # Use sweep to multiply each row by exp(intercept)
    hazard_matrix <- sweep(hazard_matrix, 1, as.vector(exp(intercept)), `*`)
  }
  # Without intercept (typical for Cox): hazard_matrix is already computed
  
  # Convert to tidy tibble format
  # hazard_matrix is (n_draws x n_times)
  # We want to arrange as: all times for draw 1, all times for draw 2, etc.
  hazard_draws_tbl <- tibble(
    time = rep(times, times = n_draws),
    draw_id = rep(1:n_draws, each = n_times),
    baseline_hazard = as.vector(t(hazard_matrix))
  )
  
  if (summary) {
    hazard_summary_tbl <- hazard_draws_tbl |>
      group_by(time) |>
      summarise(
        mean_hazard   = mean(baseline_hazard),
        median_hazard = median(baseline_hazard),
        lower_ci      = quantile(baseline_hazard, 0.025),
        upper_ci      = quantile(baseline_hazard, 0.975),
        .groups       = "drop"
        )

    return(hazard_summary_tbl)
  }

  return(hazard_draws_tbl)
}


#' Compare Baseline Hazards or Survival Curves from stan_surv and coxph Models
#'
#' Compares baseline hazard or survival estimates from a Bayesian stan_surv model 
#' and a frequentist coxph model. Returns both visualization and comparison data.
#' Supports instantaneous hazard, cumulative hazard, and survival curve comparisons.
#'
#' The comparison evaluates stan_surv's smooth M-spline baseline hazard against
#' coxph's Breslow step-function estimator. Cumulative hazard comparison is 
#' more stable and recommended as the default. Survival curves are computed from
#' cumulative hazards using S(t) = exp(-H(t)).
#'
#' @param stansurv_fit Stansurv object from rstanarm::stan_surv()
#' @param coxph_fit Coxph object from survival::coxph()
#' @param max_time Numeric: maximum time for comparison (weeks). Default Inf
#' @param plot_type Character: type of comparison - "cumulative_hazard" (default),
#'   "instantaneous_hazard", or "survival"
#' @param n Integer: number of evaluation points for stan_surv (default 1000)
#' @param prob Numeric: credible interval probability (default 0.95)
#'
#' @return List with two elements:
#'   - plot: ggplot object with comparison
#'   - data: tibble with time, stansurv estimates (median, CI), coxph values
#'
#' @examples
#' # Compare cumulative hazards up to 3 years (156 weeks)
#' comparison <- compare_baseline_hazards(
#'   stansurv_fit = lapse1_coxph_stansurv,
#'   coxph_fit = lapse1_coxph,
#'   max_time = 156,
#'   plot_type = "cumulative_hazard"
#' )
#' 
#' print(comparison$plot)
#' head(comparison$data)
#' 
#' # Compare survival curves
#' comparison2 <- compare_baseline_hazards(
#'   stansurv_fit = lapse1_coxph_stansurv,
#'   coxph_fit = lapse1_coxph,
#'   max_time = 156,
#'   plot_type = "survival"
#' )
#' 
#' # Compare instantaneous hazards
#' comparison3 <- compare_baseline_hazards(
#'   stansurv_fit = lapse1_coxph_stansurv,
#'   coxph_fit = lapse1_coxph,
#'   max_time = 156,
#'   plot_type = "instantaneous_hazard"
#' )
#'
compare_baseline_hazards <- function(stansurv_fit, 
                                    coxph_fit, 
                                    max_time = Inf,
                                    plot_type = "cumulative_hazard",
                                    n = 1000,
                                    prob = 0.95) {
  
  # Validate plot_type
  valid_types <- c("cumulative_hazard", "instantaneous_hazard", "survival")
  if (!plot_type %in% valid_types) {
    stop("plot_type must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  # Extract baseline hazard from stan_surv
  if (plot_type %in% c("cumulative_hazard", "survival")) {
    # Get all draws for cumulative hazard computation
    stansurv_hazard_tbl <- extract_stansurv_baseline_hazard(
      stansurv_fit,
      summary = FALSE,
      n = n
    )
    
    # Compute cumulative hazard for each draw
    stansurv_cumhaz <- stansurv_hazard_tbl |>
      arrange(draw_id, time) |>
      group_by(draw_id) |>
      mutate(
        time_diff = time - lag(time, default = 0),
        cumulative_hazard = cumsum(baseline_hazard * time_diff)
      ) |>
      ungroup() |>
      filter(time <= max_time)
    
    if (plot_type == "survival") {
      # Convert to survival probabilities: S(t) = exp(-H(t))
      stansurv_summary <- stansurv_cumhaz |>
        mutate(survival_prob = exp(-cumulative_hazard)) |>
        group_by(time) |>
        summarise(
          median_value = median(survival_prob),
          lower_ci = quantile(survival_prob, (1 - prob) / 2),
          upper_ci = quantile(survival_prob, (1 + prob) / 2),
          .groups = "drop"
        )
      
      y_label <- "Baseline Survival Probability"
      method_label <- "Survival Curve"
      
    } else {
      # Cumulative hazard
      stansurv_summary <- stansurv_cumhaz |>
        group_by(time) |>
        summarise(
          median_value = median(cumulative_hazard),
          lower_ci = quantile(cumulative_hazard, (1 - prob) / 2),
          upper_ci = quantile(cumulative_hazard, (1 + prob) / 2),
          .groups = "drop"
        )
      
      y_label <- "Cumulative Baseline Hazard"
      method_label <- "Cumulative Hazard"
    }
    
  } else {
    # Instantaneous hazard
    stansurv_summary <- extract_stansurv_baseline_hazard(
      stansurv_fit,
      summary = TRUE,
      n = n
    ) |>
      filter(time <= max_time) |>
      rename(
        median_value = median_hazard,
        lower_ci = lower_ci,
        upper_ci = upper_ci
      ) |>
      select(time, median_value, lower_ci, upper_ci)
    
    y_label <- "Baseline Hazard Rate"
    method_label <- "Hazard"
  }
  
  # Extract baseline hazard from coxph
  coxph_basehaz_tbl <- survival::basehaz(coxph_fit, centered = FALSE) |>
    as_tibble() |>
    rename(time = time, cumhaz = hazard) |>
    filter(time <= max_time)
  
  if (plot_type == "survival") {
    # Convert cumulative hazard to survival: S(t) = exp(-H(t))
    coxph_values <- coxph_basehaz_tbl |>
      mutate(survival_prob = exp(-cumhaz)) |>
      select(time, coxph_value = survival_prob)
      
  } else if (plot_type == "cumulative_hazard") {
    # Use cumulative hazard directly
    coxph_values <- coxph_basehaz_tbl |>
      select(time, coxph_value = cumhaz)
      
  } else {
    # Convert to instantaneous hazard
    coxph_values <- coxph_basehaz_tbl |>
      arrange(time) |>
      mutate(
        baseline_hazard = cumhaz - lag(cumhaz, default = 0),
        baseline_hazard = baseline_hazard / (time - lag(time, default = 0))
      ) |>
      select(time, coxph_value = baseline_hazard)
  }
  
  # Interpolate coxph to stan_surv time points
  coxph_interp <- tibble(
    time = stansurv_summary$time,
    coxph_value = approx(
      coxph_values$time,
      coxph_values$coxph_value,
      xout = stansurv_summary$time,
      rule = 2
    )$y
  )
  
  # Combine for comparison
  comparison_data <- stansurv_summary |>
    left_join(coxph_interp, by = "time") |>
    rename(
      stansurv_median = median_value,
      stansurv_lower = lower_ci,
      stansurv_upper = upper_ci,
      coxph_value = coxph_value
    )
  
  # Create plot
  time_label <- if (is.finite(max_time)) {
    glue::glue(" (First {max_time} weeks)")
  } else {
    ""
  }
  
  comparison_plot <- ggplot(comparison_data) +
    geom_ribbon(
      aes(x = time, ymin = stansurv_lower, ymax = stansurv_upper),
      alpha = 0.3,
      fill = "steelblue"
    ) +
    geom_line(
      aes(x = time, y = stansurv_median, color = "stan_surv (median)"),
      linewidth = 1
    ) +
    geom_line(
      aes(x = time, y = coxph_value, color = "coxph (Breslow)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    scale_color_manual(
      values = c("stan_surv (median)" = "steelblue", "coxph (Breslow)" = "darkorange"),
      name = "Method"
    ) +
    labs(
      title = glue::glue("Baseline {method_label} Comparison{time_label}"),
      x = "Time (weeks)",
      y = y_label,
      caption = glue::glue("Shaded area shows {prob * 100}% credible interval for stan_surv")
    ) +
    theme_minimal()
  
  # Return both plot and data
  list(
    plot = comparison_plot,
    data = comparison_data
  )
}


#' Extract Posterior Survival Probabilities from rstanarm Model
#'
#' Reshapes posterior survival probability matrix from rstanarm::posterior_survfit()
#' into a tidy long-format tibble suitable for analysis and visualization.
#' The input is typically a matrix with dimensions (draws × time_points).
#'
#' @param post_lst Posterior survival matrix from rstanarm::posterior_survfit()
#'   with attribute 'times' containing the time points
#'
#' @return Tibble with columns:
#'   - draw_id: Posterior draw identifier (iteration number)
#'   - idx: Original index position in the posterior matrix
#'   - value: Survival probability at this time point
#'   - time: Time value corresponding to this survival probability
#'
#' @examples
#' # Fit Cox model and extract posterior survival
#' fit <- stan_surv(Surv(time, event) ~ x1 + x2, data = data_tbl)
#' post_surv <- posterior_survfit(fit, newdata = newdata_tbl)
#'
#' # Convert to tidy format
#' post_surv_tbl <- extract_stansurv_posterior_survivals(post_surv)
#'
extract_stansurv_posterior_survivals <- function(post_lst) {
  time_vals <- post_lst |> attr('times')

  rep_count <- post_lst |> nrow()

  post_surv_tbl <- post_lst |>
    reshape2::melt() |>
    rename(
      draw_id = iterations,
      idx     = ids
    ) |>
    as_tibble() |>
    mutate(
      time = rep(time_vals, rep_count)
    )

  return(post_surv_tbl)
}


#' Interpolate Survival Probabilities to New Time Points
#'
#' Performs linear interpolation of survival curves to evaluate them at new
#' time points. This is useful when you need survival probabilities at specific
#' times not in the original posterior draws (e.g., monthly or weekly intervals).
#'
#' @param surv_data_tbl Tibble with survival data containing columns:
#'   - policy_id: Policy identifier
#'   - draw_id: Posterior draw identifier
#'   - time: Original time points
#'   - post_surv_prob: Survival probabilities at original times
#' @param new_times Numeric vector: New time points at which to evaluate survival
#'
#' @return Tibble with columns:
#'   - policy_id: Policy identifier
#'   - draw_id: Posterior draw identifier
#'   - interp_time: New interpolated time points
#'   - post_surv_prob: Interpolated survival probabilities
#'
#' @examples
#' # Interpolate survival to monthly intervals
#' monthly_times <- seq(0, 520, by = 4)  # Weeks to months
#' monthly_surv_tbl <- interpolate_survival_data(
#'   post_surv_tbl,
#'   new_times = monthly_times
#' )
#'
interpolate_survival_data <- function(surv_data_tbl, new_times) {
  interp_survdata_tbl <- surv_data_tbl |>
    reframe(
      interp_time    = new_times,
      post_surv_prob = approx(time, post_surv_prob, xout = new_times)$y,

      .by = c(policy_id, draw_id)
    )

  return(interp_survdata_tbl)
}


#' Extract Survival Time from Survival Curve Using Random Draw
#'
#' Implements inverse transform sampling to generate event times from a survival
#' curve. Given a uniform random draw u ~ U(0,1), finds the time t where S(t) = u.
#' This is the inverse of the survival CDF and is used for simulating event times.
#'
#' @param data_tbl Tibble with single survival curve containing columns:
#'   - interp_time: Time points (must be sorted)
#'   - post_surv_prob: Survival probabilities at those times
#' @param rng_draw Numeric: Random uniform draw between 0 and 1
#' @param min_time Numeric: Minimum time to return (floor value)
#'
#' @return Numeric: Simulated event time (or NA if no valid time exists)
#'
#' @examples
#' # Simulate lapse time from survival curve
#' surv_curve_tbl <- tibble(
#'   interp_time = seq(0, 520, by = 4),
#'   post_surv_prob = exp(-0.001 * interp_time)  # Exponential survival
#' )
#'
#' # Generate random lapse time
#' u <- runif(1)
#' lapse_time <- extract_survival_time(surv_curve_tbl, u, min_time = 0)
#'
#' # Simulate many lapse times
#' lapse_times <- map_dbl(runif(1000), ~ extract_survival_time(surv_curve_tbl, .x, 0))
#'
extract_survival_time <- function(data_tbl, rng_draw, min_time) {
  valid_idx <- data_tbl$post_surv_prob > rng_draw

  if (!any(valid_idx)) {
    return(NA_real_)  # No valid times (u > all survival probabilities)
  }

  # Get the last TRUE index (highest time where surv_prob > u)
  max_idx <- max(which(valid_idx))

  lapse_time <- pmax(data_tbl$interp_time[max_idx], min_time)

  return(lapse_time)
}



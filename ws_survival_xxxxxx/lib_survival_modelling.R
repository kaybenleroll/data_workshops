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
        # Policy has lapsed and lapse occurred before or at reference date
        # MUST check this first - lapse is terminal and irreversible
        ((policy_status == "lapsed") & (policy_statuschangedate <= reference_date)) ~
          "lapsed",

        # Policy has expired (term ended before reference date)
        # Term completion is also terminal
        (reference_date > policy_enddate) ~
          "completed",

        # Policy is within term and hasn't lapsed (or lapsed after reference date)
        ((reference_date >= policy_startdate) & (reference_date <= policy_enddate) &
         ((policy_status != "lapsed") | (policy_statuschangedate > reference_date))) ~
          "inforce",

        # Default case (shouldn't occur with proper data)
        TRUE ~ "unknown"
        ),

        lapsed = if_else(status_at_reference_date == "lapsed", 1L, 0L),

        weeks_to_refdate    = difftime(
          reference_date,          policy_startdate, units = "weeks"
          ) |> as.numeric(),
        weeks_to_statusdate = difftime(
          policy_statuschangedate, policy_startdate, units = "weeks"
          ) |> as.numeric(),

        policy_lifetime = if_else(
          status_at_reference_date == "lapsed",
          weeks_to_statusdate,
          weeks_to_refdate
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

        # Use appropriate age based on policy status at reference date
        age_at_observation = if_else(
          status_at_reference_date == "lapsed",
          age_at_statusdate,
          age_at_refdate
          )
      )

  return(updated_tbl)
}


#' Calculate Cashflow Predictions from Cox Model
#'
#' Generates survival probability predictions at a specified horizon and calculates
#' premium at risk for each policy.
#'
#' @param model_coxph A fitted coxph model object from the survival package
#' @param training_data_tbl A tibble containing the training data used to fit the model
#' @param prediction_horizon_weeks Numeric, the time horizon for predictions (default: 52 weeks)
#'
#' @return A list containing:
#'   - individual_predictions: tibble with policy-level predictions
#'   - aggregate_summary: tibble with aggregate totals
#'
calculate_cashflow_predictions <- function(
    model_coxph,
    training_data_tbl,
    prediction_horizon_weeks = 52
) {

  # Validate inputs
  if (!inherits(model_coxph, "coxph")) {
    stop("model_coxph must be a fitted coxph model object")
    }

  if (!is.data.frame(training_data_tbl)) {
    stop("training_data_tbl must be a data frame or tibble")
    }

  if (!is.numeric(prediction_horizon_weeks) || prediction_horizon_weeks <= 0) {
    stop("prediction_horizon_weeks must be a positive number")
    }

  # Get baseline survival function
  baseline_surv <- survfit(model_coxph)

  # Find baseline survival at prediction horizon
  # Use linear interpolation if exact time not available
  if (prediction_horizon_weeks > max(baseline_surv$time)) {
    # Extrapolate using last available value
    S0_t <- tail(baseline_surv$surv, 1)
    } else {
    # Find closest time point at or before horizon
    time_idx <- max(which(baseline_surv$time <= prediction_horizon_weeks))
    S0_t <- baseline_surv$surv[time_idx]
    }

  # Get individual risk scores (relative hazard)
  risk_scores <- predict(model_coxph, newdata = training_data_tbl, type = "risk")

  # Calculate individual survival probability at horizon
  # S(t|X) = S0(t)^exp(beta'X) = S0(t)^risk_score
  survival_prob <- S0_t^risk_scores

  # Calculate lapse probability
  lapse_prob <- 1 - survival_prob

  # Calculate premium at risk
  premium_at_risk <- training_data_tbl$prem_ape * lapse_prob

  # Create individual predictions tibble
  individual_predictions <- training_data_tbl |>
    select(policy_id, prem_ape) |>
    mutate(
      risk_score = risk_scores,
      predicted_survival_prob = survival_prob,
      predicted_lapse_prob = lapse_prob,
      predicted_premium_at_risk = premium_at_risk
      )

  # Calculate aggregate summary
  aggregate_summary <- tibble(
    total_policies = nrow(training_data_tbl),
    prediction_horizon_weeks = prediction_horizon_weeks,
    total_predicted_lapses = sum(lapse_prob),
    total_premium_at_risk = sum(premium_at_risk),
    mean_lapse_prob = mean(lapse_prob),
    median_lapse_prob = median(lapse_prob)
    )

  # Return list
  return(
    list(
      individual_predictions = individual_predictions,
      aggregate_summary = aggregate_summary
      )
    )
}


#' Compare Predicted vs Actual Lapse Outcomes
#'
#' Compares model predictions to actual outcomes in the validation period,
#' calculating error metrics and calibration statistics.
#'
#' @param predictions_list A list returned by calculate_cashflow_predictions()
#' @param actual_data_tbl A tibble containing actual outcomes with columns:
#'   policy_id, lapsed (0/1), prem_ape
#'
#' @return A list containing:
#'   - summary_metrics: overall prediction accuracy
#'   - calibration_tbl: decile-level calibration
#'   - individual_comparison: policy-level comparison data
#'
compare_predicted_vs_actual <- function(
    predictions_list,
    actual_data_tbl
) {

  # Validate inputs
  if (!is.list(predictions_list)) {
    stop("predictions_list must be a list returned by calculate_cashflow_predictions()")
    }

  if (!is.data.frame(actual_data_tbl)) {
    stop("actual_data_tbl must be a data frame or tibble")
    }

  required_cols <- c("policy_id", "lapsed", "prem_ape")
  missing_cols <- setdiff(required_cols, names(actual_data_tbl))

  if (length(missing_cols) > 0) {
    stop(paste("actual_data_tbl missing required columns:", paste(missing_cols, collapse = ", ")))
    }

  # Extract individual predictions
  individual_predictions <- predictions_list$individual_predictions

  # Join predictions with actuals
  comparison_tbl <- individual_predictions |>
    inner_join(
      actual_data_tbl |> select(policy_id, actual_lapsed = lapsed),
      by = "policy_id",
      relationship = "one-to-one"
      ) |>
    mutate(
      actual_premium_loss = prem_ape * actual_lapsed,
      prediction_error = predicted_lapse_prob - actual_lapsed
      )

  # Calculate summary metrics
  summary_metrics <- tibble(
    n_policies = nrow(comparison_tbl),
    predicted_lapses = sum(comparison_tbl$predicted_lapse_prob),
    actual_lapses = sum(comparison_tbl$actual_lapsed),
    lapse_error = predicted_lapses - actual_lapses,
    lapse_error_pct = 100 * lapse_error / actual_lapses,
    predicted_premium_loss = sum(comparison_tbl$predicted_premium_at_risk),
    actual_premium_loss = sum(comparison_tbl$actual_premium_loss),
    premium_error = predicted_premium_loss - actual_premium_loss,
    premium_error_pct = 100 * premium_error / actual_premium_loss,
    mae = mean(abs(comparison_tbl$prediction_error)),
    rmse = sqrt(mean(comparison_tbl$prediction_error^2))
    )

  # Calculate calibration by decile
  calibration_tbl <- comparison_tbl |>
    mutate(risk_decile = ntile(predicted_lapse_prob, 10)) |>
    group_by(risk_decile) |>
    summarise(
      n_policies = n(),
      mean_predicted_prob = mean(predicted_lapse_prob),
      actual_lapse_rate = mean(actual_lapsed),
      predicted_lapses = sum(predicted_lapse_prob),
      actual_lapses = sum(actual_lapsed),
      predicted_premium = sum(predicted_premium_at_risk),
      actual_premium = sum(actual_premium_loss),
      .groups = "drop"
      ) |>
    mutate(
      calibration_error = mean_predicted_prob - actual_lapse_rate,
      calibration_error_pct = 100 * calibration_error / actual_lapse_rate
      )

  # Return results
  return(
    list(
      summary_metrics = summary_metrics,
      calibration_tbl = calibration_tbl,
      individual_comparison = comparison_tbl
      )
    )
}


#' Create Cashflow Comparison Plots
#'
#' Generates visualization plots comparing predicted vs actual lapse outcomes.
#'
#' @param comparison_list A list returned by compare_predicted_vs_actual()
#'
#' @return A list of ggplot objects:
#'   - lapse_comparison_plot
#'   - premium_comparison_plot
#'   - calibration_plot
#'   - residual_plot
#'
create_cashflow_comparison_plots <- function(comparison_list) {

  # Validate input
  if (!is.list(comparison_list)) {
    stop("comparison_list must be a list returned by compare_predicted_vs_actual()")
    }

  # Extract data
  summary_metrics <- comparison_list$summary_metrics
  calibration_tbl <- comparison_list$calibration_tbl
  individual_comparison <- comparison_list$individual_comparison

  # 1. Lapse comparison plot
  lapse_comparison_data <- tibble(
    Type = c("Predicted", "Actual"),
    Lapses = c(summary_metrics$predicted_lapses, summary_metrics$actual_lapses)
    )

  lapse_comparison_plot <- ggplot(
    lapse_comparison_data,
    aes(x = Type, y = Lapses, fill = Type)
    ) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.0f", Lapses)), vjust = -0.5) +
    scale_fill_manual(values = c("Predicted" = "steelblue", "Actual" = "firebrick")) +
    labs(
      title = "Predicted vs Actual Lapses",
      subtitle = sprintf(
        "Error: %.1f%%",
        summary_metrics$lapse_error_pct
        ),
      x = NULL,
      y = "Number of Lapses"
      ) +
    theme(legend.position = "none")

  # 2. Premium comparison plot
  premium_comparison_data <- tibble(
    Type = c("Predicted", "Actual"),
    Premium = c(
      summary_metrics$predicted_premium_loss,
      summary_metrics$actual_premium_loss
      )
    )

  premium_comparison_plot <- ggplot(
    premium_comparison_data,
    aes(x = Type, y = Premium, fill = Type)
    ) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(aes(label = sprintf("£%.0f", Premium)), vjust = -0.5) +
    scale_fill_manual(values = c("Predicted" = "steelblue", "Actual" = "firebrick")) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Predicted vs Actual Premium Loss",
      subtitle = sprintf(
        "Error: %.1f%%",
        summary_metrics$premium_error_pct
        ),
      x = NULL,
      y = "Premium Loss (£)"
      ) +
    theme(legend.position = "none")

  # 3. Calibration plot
  calibration_plot <- ggplot(
    calibration_tbl,
    aes(x = mean_predicted_prob, y = actual_lapse_rate)
    ) +
    geom_point(aes(size = n_policies), alpha = 0.6, colour = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_smooth(method = "lm", se = TRUE, colour = "firebrick", linewidth = 0.8) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = "Calibration Plot: Predicted vs Actual Lapse Rate by Decile",
      subtitle = "Points should fall near diagonal for perfect calibration",
      x = "Mean Predicted Lapse Probability",
      y = "Actual Lapse Rate",
      size = "Policies"
      ) +
    coord_fixed()

  # 4. Residual plot
  residual_plot <- ggplot(
    individual_comparison,
    aes(x = prediction_error)
    ) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "firebrick") +
    labs(
      title = "Distribution of Prediction Errors",
      subtitle = sprintf(
        "MAE: %.4f, RMSE: %.4f",
        summary_metrics$mae,
        summary_metrics$rmse
        ),
      x = "Prediction Error (Predicted - Actual)",
      y = "Count"
      )

  # Return list of plots
  return(
    list(
      lapse_comparison_plot = lapse_comparison_plot,
      premium_comparison_plot = premium_comparison_plot,
      calibration_plot = calibration_plot,
      residual_plot = residual_plot
      )
    )
}



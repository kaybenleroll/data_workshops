#' Create Convergence Diagnostic Plots for rstanarm Objects
#'
#' Produces convergence-focused diagnostic plots including Rhat, ESS ratio,
#' and a text summary of sampling diagnostics.
#'
#' @param stanfit A fitted stanarm object (e.g., from stan_surv())
#' @param pars Optional character vector of parameter names.
#'   If NULL, automatically uses all non-baseline-hazard coefficients.
#' @param title Optional character string for overall plot title
#'
#' @return A cowplot grid object containing convergence diagnostic plots
#'
#' @details
#' The function checks for:
#' - Rhat < 1.01 (convergence criterion)
#' - ESS ratio > 0.1 (minimum), > 0.5 (ideal)
#' - Divergent transitions (should be 0)
#' - Maximum treedepth hits (should be rare)
#'
#' @examples
#' plot_stan_convergence(lapse1_coxph_stansurv)
#' plot_stan_convergence(lapse1_coxph_stansurv, title = "Model 1 Convergence")
#'
#' @export
plot_stan_convergence <- function(stanfit, pars = NULL, title = NULL) {

  # Extract parameter names if not specified
  if (is.null(pars)) {
    # Get coefficient names, excluding baseline hazard parameters
    all_pars <- names(coef(stanfit))
    pars <- all_pars[!grepl("^sbhaz", all_pars)]
  }

  # 1. Rhat diagnostic (should all be < 1.01)
  p1 <- stanfit |>
    rhat() |>
    mcmc_rhat() +
    geom_vline(xintercept = 1.01, linetype = "dashed", color = "red", alpha = 0.7) +
    labs(
      title = "Rhat Values",
      subtitle = "Should be < 1.01 (dashed line)"
      ) +
    theme_cowplot(font_size = 10)

  # 2. Effective sample size ratio (should be > 0.1, ideally > 0.5)
  p2 <- stanfit |>
    neff_ratio() |>
    mcmc_neff() +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "red", alpha = 0.7) +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "blue", alpha = 0.7) +
    labs(
      title = "ESS Ratio",
      subtitle = "Should be > 0.1 (red), ideally > 0.5 (blue)"
      ) +
    theme_cowplot(font_size = 10)

  # 3. Check for divergences and create summary text
  np <- nuts_params(stanfit)

  # Count diagnostics
  n_divergent <- np |>
    filter(Parameter == "divergent__", Value == 1) |>
    nrow()
  n_max_tree <- np |>
    filter(Parameter == "treedepth__", Value >= 10) |>
    nrow()

  # Create a text plot with diagnostic summary
  diagnostic_text <- glue(
    "MCMC Diagnostics Summary\n",
    "========================\n\n",
    "Chains: {stanfit$stanfit@sim$chains}\n",
    "Iterations: {stanfit$stanfit@sim$iter}\n",
    "Warmup: {stanfit$stanfit@sim$warmup}\n\n",
    "Divergent transitions: {n_divergent}\n",
    "Max treedepth hits: {n_max_tree}\n\n",
    "Parameters monitored: {length(pars)}"
  )

  # Create empty plot with text (smaller font)
  p4 <- ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = diagnostic_text,
      hjust = 0.5, vjust = 0.5,
      family = "mono",
      size = 2.2
      ) +
    theme_void() +
    labs(title = "Sampling Diagnostics")

  # 4. Alternative diagnostic panel: divergences per chain (bottom-right)
  div_tbl <- np |>
    dplyr::filter(Parameter == "divergent__") |>
    dplyr::group_by(Chain) |>
    dplyr::summarise(n_div = sum(Value == 1), .groups = "drop")

  p3 <- div_tbl |>
    ggplot(aes(x = factor(Chain), y = n_div)) +
    geom_col(fill = "#e57373") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    labs(
      title = "Divergences per Chain",
      x = "Chain",
      y = "Count"
      ) +
    theme_cowplot(font_size = 10)

  # Combine plots in a 2x2 grid:
  # Top row: Rhat (p1), ESS Ratio (p2)
  # Bottom row: Divergences per chain (p3), Diagnostic text (p4)
  final_plot <- plot_grid(
    p1, p2, p3, p4,
    ncol = 2,
    rel_widths = c(1, 1),
    rel_heights = c(1, 1)
    )

  # Add overall title if provided
  if (!is.null(title)) {
    title_theme <- ggdraw() +
      draw_label(title, fontface = "bold", size = 14)

    final_plot <- plot_grid(
      title_theme,
      final_plot,
      ncol = 1,
      rel_heights = c(0.1, 1)
      )
  }

  return(final_plot)
}


#' Create Chain Mixing Diagnostic Plots for rstanarm Objects
#'
#' Produces chain mixing diagnostic plots including trace plots, density overlays,
#' and autocorrelation plots.
#'
#' @param stanfit A fitted stanarm object (e.g., from stan_surv())
#' @param pars Optional character vector of parameter names to highlight.
#'   If NULL, automatically selects the 6 parameters with lowest n_eff ratio
#'   (excluding baseline hazard parameters), as these are most likely to have
#'   mixing issues.
#' @param title Optional character string for overall plot title
#'
#' @return A cowplot grid object containing chain mixing diagnostic plots
#'
#' @details
#' The function creates:
#' - Trace plots showing MCMC chains over iterations
#' - Posterior density overlays by chain
#' - Autocorrelation plots for first 4 parameters
#'
#' Good mixing shows:
#' - Trace plots with no trends or stuck chains
#' - Overlapping densities across chains
#' - Rapidly decaying autocorrelation
#'
#' When pars is NULL, the function automatically identifies parameters with
#' the lowest effective sample size ratios, which are most likely to exhibit
#' mixing problems.
#'
#' @examples
#' plot_stan_mixing(lapse1_coxph_stansurv)
#' plot_stan_mixing(lapse1_coxph_stansurv, title = "Model 1 Chain Mixing")
#'
#' @export
plot_stan_mixing <- function(stanfit, pars = NULL, title = NULL) {

  # Extract parameter names if not specified
  if (is.null(pars)) {
    # Get coefficient names, excluding baseline hazard parameters
    all_pars <- names(coef(stanfit))
    candidate_pars <- all_pars[!grepl("^sbhaz", all_pars)]

    # Get n_eff ratios for these parameters
    neff_ratios <- neff_ratio(stanfit, pars = candidate_pars)

    # Sort by n_eff ratio (lowest first) and take top 6
    sorted_pars <- names(sort(neff_ratios))
    pars <- sorted_pars[1:min(6, length(sorted_pars))]

    pars_list <- paste(pars, collapse = ", ")
    pars_wrapped <- strwrap(pars_list, width = 80, prefix = "  ")
    message(glue("Automatically selected 6 parameters with lowest n_eff ratios:"))
    message(paste(pars_wrapped, collapse = "\n"))
  } else {
    # Limit to first 6 parameters if too many (for readability)
    if (length(pars) > 6) {
      message(glue("Too many parameters ({length(pars)}), showing first 6 only"))
      pars <- pars[1:6]
    }
  }

  # 1. Trace plots for main parameters
  p1 <- mcmc_trace(stanfit, pars = pars, facet_args = list(ncol = 2)) +
    labs(title = "Trace Plots (MCMC chains)") +
    theme_cowplot(font_size = 11)

  # 2. Density overlay by chain
  p2 <- mcmc_dens_overlay(stanfit, pars = pars, facet_args = list(ncol = 2)) +
    labs(title = "Posterior Densities by Chain") +
    theme_cowplot(font_size = 11)

  # 3. Autocorrelation for first few parameters
  if (length(pars) >= 2) {
    p3 <- mcmc_acf(stanfit, pars = pars[1:min(4, length(pars))], lags = 20) +
      labs(title = "Autocorrelation") +
      theme_cowplot(font_size = 11)
  } else {
    p3 <- ggplot() + theme_void()
  }

  # Combine plots using cowplot
  top_row <- plot_grid(p1, p2, ncol = 2)
  bottom_row <- plot_grid(p3, ncol = 1)

  final_plot <- plot_grid(
    top_row,
    bottom_row,
    ncol = 1,
    rel_heights = c(1.5, 1)
    )

  # Add overall title if provided
  if (!is.null(title)) {
    title_theme <- ggdraw() +
      draw_label(title, fontface = "bold", size = 14)

    final_plot <- plot_grid(
      title_theme,
      final_plot,
      ncol = 1,
      rel_heights = c(0.1, 1)
      )
  }

  return(final_plot)
}

#' Resolve Namespace Conflicts Between Packages
#'
#' Configures conflict resolution preferences for commonly conflicting functions
#' using a priority system. When multiple packages export the same function name,
#' this function selects the package highest in the priority list using the
#' conflicted package.
#'
#' @param pkg_priority Character vector: Package names in order of priority
#'   (first package has highest priority)
#'
#' @return Named list showing all conflicts found (returned invisibly)
#'
#' @examples
#' # Give tidyverse packages priority over base R
#' resolve_conflicts(c("dplyr", "lubridate", "stats"))
#' # Now filter() will use dplyr::filter, lag() uses dplyr::lag, etc.
#'
resolve_conflicts <- function(pkg_priority) {
  get_index <- function(pkg_name) {
    idx <- str_which(pkg_priority, pkg_name)

    if(length(idx) == 0) { idx <- 0L }

    return(idx)
  }

  conflict_lst <- conflict_scout()

  for(func_name in names(conflict_lst)) {
    pkg_index <- map_int(conflict_lst[[func_name]], get_index)

    pkg_index <- pkg_index[pkg_index > 0]

    if(length(pkg_index) == 0) {
      pkg_use <- conflict_lst[[func_name]][1]
    } else {
      pkg_use <- pkg_priority[pkg_index |> min()]
    }

    conflict_prefer(func_name, pkg_use)
  }

  return(conflict_lst)
}

#' Convert Gamma Distribution Parameters from Mean/CV to Shape/Rate
#'
#' Converts gamma distribution parameterization from mean (mu) and coefficient
#' of variation (CV) to the standard shape (alpha) and rate (beta) parameters.
#'
#' @param mu Numeric: Mean of the gamma distribution (mu > 0)
#' @param cv Numeric: Coefficient of variation (CV = sd/mean, cv > 0)
#'
#' @return Named numeric vector with two elements:
#'   - shape: Shape parameter (alpha = 1/cv^2)
#'   - rate: Rate parameter (beta = 1/(mu * cv^2))
#'
#' @examples
#' # Gamma distribution with mean=10, CV=0.5
#' params <- gamma_mucv2shaperate(mu = 10, cv = 0.5)
#' # Returns: shape = 4, rate = 0.4
#'
#' # Can be used with rgamma:
#' samples <- rgamma(1000, shape = params["shape"], rate = params["rate"])
#' mean(samples)  # Should be approximately 10
#'
gamma_mucv2shaperate <- function(mu, cv) {
  shape <- 1 / (cv^2)
  rate  <- 1 / (cv^2 * mu)

  return(c(shape = shape, rate = rate))
}


#' Convert Gamma Distribution Parameters from Shape/Rate to Mean/CV
#'
#' Converts gamma distribution parameterization from standard shape (alpha) and
#' rate (beta) parameters to mean (mu) and coefficient of variation (CV).
#'
#' @param shape Numeric: Shape parameter (alpha > 0)
#' @param rate Numeric: Rate parameter (beta > 0)
#'
#' @return A named list with two elements:
#'   - mu: Mean of the distribution (mu = shape/rate)
#'   - cv: Coefficient of variation (cv = 1/sqrt(shape))
#'
#' @examples
#' # Standard gamma(shape=4, rate=0.4)
#' params <- gamma_shaperate2mucv(shape = 4, rate = 0.4)
#' # Returns: mu = 10, cv = 0.5
#'
gamma_shaperate2mucv <- function(shape, rate) {
  mu <- shape / rate
  cv <- 1 / sqrt(shape)

  return(c(mu = mu, cv = cv))
}


#' Generate Gamma Random Variables Using Mean/CV Parameterization
#'
#' Generates random variates from a gamma distribution specified by its mean
#' and coefficient of variation rather than shape/rate parameters.
#'
#' @param n Integer: Number of random variates to generate
#' @param mu Numeric: Mean of the gamma distribution (mu > 0)
#' @param cv Numeric: Coefficient of variation (CV = sd/mean, cv > 0)
#'
#' @return Numeric vector of length n containing gamma-distributed random values
#'
#' @examples
#' # Generate 1000 samples from Gamma with mean=10, CV=0.5
#' samples <- rgamma_mucv(1000, mu = 10, cv = 0.5)
#' mean(samples)  # Should be approximately 10
#' sd(samples) / mean(samples)  # Should be approximately 0.5
#'
rgamma_mucv <- function(n, mu, cv) {
  params <- gamma_mucv2shaperate(mu, cv)

  rgamma(n = n, shape = params[1], rate = params[2], ...)
}


#' Calculate Distribution Q-Values Using ECDF
#'
#' Computes empirical cumulative distribution function (ECDF) quantile values
#' for reference values within grouped data. Useful for calculating p-values
#' or quantile positions of observed values within posterior/simulation distributions.
#'
#' @param data_tbl Tibble: Data containing distribution values and reference values
#' @param distrib_vals Column name (unquoted): Distribution values to build ECDF from
#' @param ref_val Column name (unquoted): Reference values to evaluate within ECDF
#' @param ... Grouping variables (unquoted): Variables to group by before computing q-values
#'
#' @return Tibble with grouping variables, ref_val, and computed q_val (quantile position)
#'
#' @examples
#' # Calculate where observed values fall in posterior distributions
#' posterior_tbl <- tibble(
#'   parameter = rep(c("beta1", "beta2"), each = 1000),
#'   posterior_draw = c(rnorm(1000, 0.5, 0.1), rnorm(1000, -0.3, 0.15)),
#'   observed = rep(c(0.48, -0.35), each = 1000)
#' )
#'
#' calculate_distribution_qvals(
#'   posterior_tbl,
#'   distrib_vals = posterior_draw,
#'   ref_val = observed,
#'   parameter
#' )
#' # Returns q-values showing where observed falls in posterior (like a p-value)
#'
calculate_distribution_qvals <- function(data_tbl, distrib_vals, ref_val, ...) {
  qval_data_tbl <- data_tbl |>
    group_by(..., {{ ref_val }}) |>
    summarise(
      .groups = "drop",
      dval_lst = list({{ distrib_vals }})
      ) |>
    mutate(
      q_val = map2_dbl(dval_lst, {{ref_val}}, ~ ecdf(.x)(.y))
      )

  return(qval_data_tbl)
}


#' Ensure Precompute Directory Exists with Git Ignore
#'
#' Creates a directory for storing precomputed results (cached data, fitted
#' models, etc.) and adds a .gitignore file to prevent committing these files
#' to version control. Useful for managing large intermediate results.
#'
#' @param precompute_path Character: Path to the precompute directory to create
#'
#' @return NULL (invisible). Function called for side effects (directory creation).
#'
#' @examples
#' # Create directory for caching model fits
#' ensure_exists_precompute_directory("fitted_models")
#'
#' # Create directory for simulation results
#' ensure_exists_precompute_directory("simulation_output")
#'
ensure_exists_precompute_directory <- function(precompute_path) {
  if(dir_exists(precompute_path)) {
    return(TRUE)
  }

  gitignore_path <- glue("{precompute_path}/.gitignore")

  dir_create(precompute_path)

  write_lines("*.rds",     gitignore_path)
  write_lines("*.qs",      gitignore_path)
  write_lines("*.parquet", gitignore_path)

  return(FALSE)
}


#' Write Parquet File with Default Compression
#'
#' Wrapper around arrow::write_parquet that sets zstd compression at level 3
#' by default for optimal balance between file size and write performance.
#' All other arguments are passed through to arrow::write_parquet.
#'
#' @param x Data frame or tibble to write
#' @param sink File path or connection to write to
#' @param ... Additional arguments passed to arrow::write_parquet
#'   (compression and compression_level can be overridden)
#'
#' @return NULL (invisible). Function called for side effect of writing file.
#'
#' @examples
#' # Write with default zstd level 3 compression
#' data_tbl |> write_parquet_compressed("output.parquet")
#'
#' # Override compression level if needed
#' data_tbl |> write_parquet_compressed("output.parquet", compression_level = 5)
#'
#' # Use different compression algorithm
#' data_tbl |> write_parquet_compressed("output.parquet", compression = "snappy")
#'
write_parquet_compressed <- function(x, sink, ...) {
  write_parquet(
    x, sink,
    compression = "zstd",
    compression_level = 3,
    ...
  )
}

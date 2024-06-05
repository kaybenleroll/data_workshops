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

gamma_mucv2shaperate <- function(mu, cv) {
  shape <- 1 / (cv^2)
  rate  <- 1 / (cv^2 * mu)

  return(c(shape = shape, rate = rate))
}


gamma_shaperate2mucv <- function(shape, rate) {
  mu <- shape / rate
  cv <- 1 / sqrt(shape)

  return(c(mu = mu, cv = cv))
}


rgamma_mucv <- function(n, mu, cv, ...) {
  params <- gamma_mucv2shaperate(mu, cv)

  rgamma(n = n, shape = params[1], rate = params[2], ...)
}


calculate_distribution_qvals <- function(data_tbl, distrib_vals, ref_val, ...) {
  qval_data_tbl <- data_tbl |>
    group_by(..., {{ ref_val }}) |>
    summarise(
      .groups = "drop",

      dval_lst = list({{ distrib_vals }})
      ) %>%
    mutate(
      q_val = map2_dbl(dval_lst, {{ref_val}}, ~ ecdf(.x)(.y))
      )

  return(qval_data_tbl)
}


ensure_exists_precompute_directory <- function(precompute_path) {
  if(dir_exists(precompute_path)) {
    return(TRUE)
  }

  gitignore_path <- glue("{precompute_path}/.gitignore")

  dir_create(precompute_path)

  write_lines("*.rds", gitignore_path)

  return(FALSE)
}

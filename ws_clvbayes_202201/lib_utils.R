resolve_conflicts <- function(pkg_priority) {
  get_index <- function(pkg_name) {
    idx <- str_which(pkg_priority, pkg_name)

    if(length(idx) == 0) {
      idx <- 0L
    }

    return(idx)
  }

  conflict_lst <- conflict_scout()

  for(func_name in names(conflict_lst)) {
    pkg_index <- map_int(conflict_lst[[func_name]], get_index)

    pkg_index <- pkg_index[pkg_index > 0]

    if(length(pkg_index) == 0) {
      pkg_use <- conflict_lst[[func_name]][1]
    } else {
      pkg_use <- pkg_index %>%
        min() %>%
        pkg_priority[.]

    }

    conflict_prefer(func_name, pkg_use)
  }

  return(conflict_lst)
}


calculate_distribution_qvals <- function(data_tbl, distrib_vals, ref_val, ...) {
  qval_data_tbl <- data_tbl %>%
    group_by(..., {{ ref_val }}) %>%
    summarise(
      .groups = "drop",

      dval_lst = list({{ distrib_vals }})
    ) %>%
    mutate(
      q_val = map2_dbl(dval_lst, {{ref_val}}, ~ ecdf(.x)(.y))
    )

  return(qval_data_tbl)
}


calculate_medical_summary_stats <- function(population_tbl) {

  n_truepos <- population_tbl %>%
    filter(infected == TRUE, test_result == TRUE) %>%
    nrow()

  n_trueneg <- population_tbl %>%
    filter(infected == FALSE, test_result == FALSE) %>%
    nrow()

  n_falsepos <- population_tbl %>%
    filter(infected == FALSE, test_result == TRUE) %>%
    nrow()

  n_falseneg <- population_tbl %>%
    filter(infected == TRUE, test_result == FALSE) %>%
    nrow()


  prop_falsepos <- n_falsepos / (n_falsepos + n_truepos)
  prop_falseneg <- n_falseneg / (n_falseneg + n_trueneg)

  summstat_lst <- list(
    prop_falsepos = prop_falsepos,
    prop_falseneg = prop_falseneg
    )

  return(summstat_lst)
}

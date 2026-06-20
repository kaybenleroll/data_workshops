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



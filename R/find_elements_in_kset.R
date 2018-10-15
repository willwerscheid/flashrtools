find_factor_elements_in_kset <- function(fl,
                                         elem_names,
                                         cutoff = 0.05) {
  return(find_elements_in_kset(fl, elem_names, cutoff, "factors"))
}

find_loading_elements_in_kset <- function(fl,
                                          elem_names,
                                          cutoff = 0.05) {
  return(find_elements_in_kset(fl, elem_names, cutoff, "loadings"))
}

find_elements_in_kset <- function(fl,
                                  elem_names,
                                  cutoff,
                                  where = c("loadings", "factors")) {
  where <- match.arg(where)

  vals <- switch(where,
                 loadings = fl$ldf$l,
                 factors = fl$ldf$f)
  ret <- list()

  for (elem_name in elem_names) {
    if (!(elem_name %in% row.names(vals))) {
      warning("Element ", elem_name, " not found.")
    } else {
      elem_vals <- vals[elem_name, ]
      ret[[elem_name]] <- which(elem_vals > cutoff)
    }
  }

  return(ret)
}

get_top_factor_elements <- function(fl,
                                    kset = 1:fl$nfactors,
                                    min_elem = 1,
                                    max_elem = 20,
                                    cutoff = 0.05) {
  return(get_top_elements(fl, kset, min_elem, max_elem, cutoff, "factors"))
}

get_top_loading_elements <- function(fl,
                                     kset = 1:fl$nfactors,
                                     min_elem = 1,
                                     max_elem = 20,
                                     cutoff = 0.05) {
  return(get_top_elements(fl, kset, min_elem, max_elem, cutoff, "loadings"))
}

get_top_elements <- function(fl,
                             kset,
                             min_elem,
                             max_elem,
                             cutoff,
                             where = c("loadings", "factors")) {
  where <- match.arg(where)

  ret <- list()
  for (k in kset) {
    vals <- switch(where,
                   loadings = fl$ldf$l[, k],
                   factors = fl$ldf$f[, k])

    top_vals <- sort(abs(vals), decreasing = TRUE)[1:max_elem]
    top_vals <- c(top_vals[1:min_elem],
                  top_vals[-(1:min_elem)][top_vals[-(1:min_elem)] > cutoff])

    ret[[as.character(k)]] <- top_vals
  }

  return(ret)
}

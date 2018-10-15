flashier <- function(data,
                     Kmax = 100,
                     f_init = NULL,
                     var_type = c("by_column",
                                  "by_row",
                                  "constant",
                                  "zero",
                                  "kroneker"),
                     method = c("fastest",
                                "nnfactors",
                                "nnloadings",
                                "custom"),
                     greedy = TRUE,
                     backfit = FALSE,
                     nullcheck = TRUE,
                     verbose = TRUE,
                     init_fn = NULL,
                     ebnm_fn = NULL,
                     ebnm_param = NULL,
                     stopping_rule = NULL,
                     tol = NULL,
                     verbose_output = NULL) {
  method <- match.arg(method)

  default <- get_method_defaults(method)
  init_fn <- override_default(default$init_fn, init_fn)
  ebnm_fn <- override_default(default$ebnm_fn, ebnm_fn)
  ebnm_param <- override_default(default$ebnm_param, ebnm_param)
  stopping_rule <- override_default(default$stopping_rule, stopping_rule)
  tol <- override_default(default$tol, tol)
  verbose_output <- override_default(default$verbose_output, verbose_output)

  if (!verbose) {
    verbose_output <- ""
  }

  if (greedy) {
    fl <- flashr:::flash_greedy_workhorse(data = data,
                                          Kmax = Kmax,
                                          f_init = f_init,
                                          var_type = var_type,
                                          init_fn = init_fn,
                                          ebnm_fn = ebnm_fn,
                                          ebnm_param = ebnm_param,
                                          stopping_rule = stopping_rule,
                                          tol = tol,
                                          verbose_output = verbose_output,
                                          nullcheck = nullcheck,
                                          seed = 666)
  } else {
    fl <- f_init
  }

  if (backfit) {
    fl <- flashr::flash_backfit_workhorse(data = data,
                                          f_init = fl,
                                          var_type = var_type,
                                          ebnm_fn = ebnm_fn,
                                          ebnm_param = ebnm_param,
                                          stopping_rule = stopping_rule,
                                          tol = tol,
                                          verbose_output = verbose_output,
                                          nullcheck = nullcheck)
  }

  return(fl)
}

override_default <- function(default, override) {
  if (is.null(override)) {
    return(default)
  } else {
    return(override)
  }
}

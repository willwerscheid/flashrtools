flashier <- function(data,
                     f_init = NULL,
                     var_type = c("by_column",
                                  "by_row",
                                  "constant",
                                  "zero",
                                  "kroneker"),
                     method = c("fastest",
                                "nonnegative",
                                "nnfactors",
                                "nnloadings",
                                "custom"),
                     greedy_Kmax = 100,
                     greedy_maxiter = 500,
                     backfit_maxiter = 0,
                     nullcheck = TRUE,
                     verbose = TRUE,
                     custom_params = list()) {
  method <- match.arg(method)

  params <- get_method_defaults(method)
  params <- modifyList(params, custom_params, keep.null = TRUE)

  if (!verbose) {
    params$verbose_output <- ""
  }

  if (greedy_Kmax > 0 && greedy_maxiter > 0) {
    fl <- flashr:::flash_greedy_workhorse(data = data,
                                          Kmax = greedy_Kmax,
                                          f_init = f_init,
                                          var_type = var_type,
                                          init_fn = params$init_fn,
                                          ebnm_fn = params$ebnm_fn,
                                          ebnm_param = params$ebnm_param,
                                          stopping_rule = params$stopping_rule,
                                          tol = params$tol,
                                          verbose_output = params$verbose_output,
                                          nullcheck = nullcheck,
                                          maxiter = greedy_maxiter,
                                          seed = 666)
  } else {
    fl <- f_init
  }

  if (backfit_maxiter > 0) {
    fl <- flashr:::flash_backfit_workhorse(data = data,
                                           f_init = fl,
                                           var_type = var_type,
                                           ebnm_fn = params$ebnm_fn,
                                           ebnm_param = params$ebnm_param,
                                           stopping_rule = params$stopping_rule,
                                           tol = params$tol,
                                           verbose_output = params$verbose_output,
                                           nullcheck = nullcheck,
                                           maxiter = backfit_maxiter)
  }

  return(fl)
}

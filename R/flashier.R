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
                     verbose_output = NULL,
                     tol = 1e-2) {

  method <- match.arg(method)
  init_fn <- get_init_fn(method, init_fn)
  ebnm_fn <- get_ebnm_fn(method, ebnm_fn)
  ebnm_param <- get_ebnm_param(method, ebnm_param)
  stopping_rule <- get_stopping_rule(method, stopping_rule)
  verbose_output <- ifelse(verbose,
                           get_verbose_output(method, verbose_output),
                           "")

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

get_init_fn <- function(method, custom_init_fn) {
  return(switch(method,
                fastest = "udv_si",
                nnloadings = "udv_nnloadings",
                nnfactors = "udv_nnfactors",
                custom = custom_init_fn))
}

get_ebnm_fn <- function(method, custom_ebnm_fn) {
  return(switch(method,
                fastest = "ebnm_pn",
                nnloadings = "ebnm_ash",
                nnfactors = "ebnm_ash",
                custom = custom_ebnm_fn))
}

get_ebnm_param <- function(method, custom_ebnm_param) {
  nn_param <- list(mixcompdist = "+uniform",
                   optmethod = "mixSQP")
  ash_param <- list(mixcompdist = "normal",
                    optmethod = "mixSQP")

  return(switch(method,
                fastest = NULL,
                nnloadings = list(l = nn_param, f = ash_param),
                nnfactors = list(l = ash_param, l = nn_param),
                custom = custom_ebnm_param))
}

get_stopping_rule <- function(method, custom_stopping_rule) {
  return(switch(method,
                fastest = "objective",
                nnloadings = "loadings",
                nnfactors = "factors",
                custom = custom_stopping_rule))
}

get_verbose_output <- function(method, custom_verbose_output) {
  return(switch(method,
                fastest = "od",
                nnloadings = "odL",
                nnfactors = "odF",
                custom = custom_verbose_output))
}

context("Flashier interface")

set.seed(666)

n <- 4
p <- 5
k <- 2
LL <- matrix(0, nrow = n, ncol = k)
LL[1:2, 1] <- -10
LL[2:4, 2] <- 15
FF <- matrix(0, nrow = p, ncol = k)
FF[1:3, 1] <- 1:3
FF[3:5, 2] <- 1:3
Y <- LL %*% t(FF) + matrix(rnorm(n * p), nrow = n, ncol = p)
fl_data <- flash_set_data(Y, S = 1)

test_that("fastest method works (greedy and backfit)", {
  fl <- flashier(fl_data,
                 var_type = "zero",
                 method = "fastest",
                 greedy_Kmax = 1)
  expect_equal(fl$nfactors, 1)
  fl2 <- flashier(fl_data,
                  f_init = fl,
                  var_type = "zero",
                  method = "fastest",
                  greedy_Kmax = 1,
                  greedy_maxiter = 1,
                  backfit_maxiter = 1,
                  nullcheck = FALSE)
  expect_equal(fl2$nfactors, 2)
})

test_that("custom method works (greedy and backfit)", {
  custom_params <- list(init_fn = "udv_svd",
                        ebnm_fn = list(l = "ebnm_pn", f = "ebnm_ash"),
                        ebnm_param = list(l = list(),
                                          f = list(mixcompdist = "normal")),
                        stopping_rule = "objective",
                        tol = 1e-1,
                        verbose_output = "dln")
  fl_cust <- flashier(fl_data,
                      var_type = "zero",
                      method = "custom",
                      custom_params = custom_params,
                      backfit_maxiter = 3)
  expect_equal(fl_cust$fit$ebnm_fn_l[[1]], "ebnm_pn")
  expect_equal(fl_cust$fit$ebnm_fn_f[[1]], "ebnm_ash")
})

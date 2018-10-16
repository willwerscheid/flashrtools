context("Nonnegative methods")

set.seed(666)

n <- 4
p <- 5
LL <- c(10, 10, 10, -10)
FF <- c(10, 10, 10, 10, -10)
Y <- outer(LL, FF) + matrix(rnorm(n * p), nrow = n, ncol = p)

test_that("nnfactors method returns nonnegative factors", {
  fl_nnf <- flashier(Y, var_type = "constant", method = "nnfactors")
  expect_true(all(fl_nnf$ldf$f >= 0))
})

test_that("nnloadings method returns nonnegative loadings", {
  fl_nnl <- flashier(Y, var_type = "constant", method = "nnloadings")
  expect_true(all(fl_nnl$ldf$l >= 0))
})

test_that("nonnegative method returns nonnegative factor/loading pairs", {
  Y <- pmax(Y, 0)
  fl_nn <- flashier(Y, var_type = "constant", method = "nonnegative")
  expect_true(all(fl_nn$ldf$f >= 0))
  expect_true(all(fl_nn$ldf$l >= 0))
})

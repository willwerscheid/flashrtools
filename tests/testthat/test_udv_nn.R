context("udv_nn")

set.seed(666)

n <- 3
p <- 5
Y <- matrix(rnorm(n * p), nrow = n, ncol = p)

test_that("udv_nnfactors returns a nonnegative factor", {
  udv_nnf <- udv_nnfactors(Y)
  expect_named(udv_nnf, c("u", "d", "v"))
  expect_true(all(udv_nnf$u >= 0))
})

test_that("udv_nnloadings returns a nonnegative loading", {
  udv_nnl <- udv_nnloadings(Y)
  expect_named(udv_nnl, c("u", "d", "v"))
  expect_true(all(udv_nnl$v >= 0))
})

test_that("udv_nn errors out when K > 1", {
  expect_error(udv_nnfactors(Y, K = 2))
})

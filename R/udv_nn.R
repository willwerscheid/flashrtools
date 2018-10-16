# Initialize a nonnegative factor/loading pair via NNLM.
#
udv_nn <- function(Y, K = 1) {
  WH <- NNLM::nnmf(Y, K, verbose = 0)
  return(list(u = WH$W, d = rep(1, K), v = WH$H))
}

# Initialize a nonnegative factor (with no constraints on loadings).
#
udv_nnfactors <- function(Y, K = 1) {
  return(udv_partialnn(Y, K, nn = "factors"))
}

# Initialize a nonnegative loading vector (with no constraints on factors).
#
udv_nnloadings <- function(Y, K = 1) {
  return(udv_partialnn(Y, K, nn = "loadings"))
}

udv_partialnn <- function(Y, K = 1, nn = c("factors", "loadings")) {
  if (K > 1) {
    stop(paste("K > 1 not yet implemented for nonnegative initialization",
               "functions"))
  }
  nn <- match.arg(nn)

  udv <- udv_si(Y, K)
  vector_to_check <- switch(nn, factors = udv$u, loadings = udv$v)
  if (is_neg_part_larger(vector_to_check)) {
    udv$u <- -udv$u
    udv$v <- -udv$v
  }

  if (identical(nn, "factors")) {
    udv$u <- get_pos_part(udv$u)
  } else if (identical(nn, "loadings")) {
    udv$v <- get_pos_part(udv$v)
  }

  return(udv)
}

is_neg_part_larger <- function(x) {
  pos_part <- x[x > 0]
  neg_part <- x[x < 0]
  return(sum(neg_part^2) > sum(pos_part^2))
}

get_pos_part <- function(x) {
  x[x < 0] <- 0
  return(x)
}

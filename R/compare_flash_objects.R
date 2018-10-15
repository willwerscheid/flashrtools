find_similar_factors <- function(fl1, fl2, threshold = 0.9) {
  return(find_similar(fl1, fl2, threshold, "factors"))
}

find_similar_loadings <- function(fl1, fl2, threshold = 0.9) {
  return(find_similar(fl1, fl2, threshold, "loadings"))
}

find_similar <- function(fl1, fl2, threshold,
                         comparison = c("factors", "loadings")) {
  comparison = match.arg(comparison)

  X1 <- switch(comparison,
               factors = fl1$ldf$f,
               loadings = fl1$ldf$l)
  X2 <- switch(comparison,
               factors = fl2$ldf$f,
               loadings = fl2$ldf$l)
  cos_sim <- t(X1) %*% X2

  best_match <- apply(cos_sim, 1, which.max)
  sim_score <- apply(cos_sim, 1, max)
  idx <- which(sim_score > threshold)

  return(data.frame(k1 = idx,
                    k2 = best_match[idx],
                    sim_score = sim_score[idx]))
}

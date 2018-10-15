plot_factor_wordclouds <- function(fl,
                                   kset,
                                   scale = c(2, .1),
                                   max_words = 50,
                                   rot_per = .2,
                                   brewer_pal = "Blues") {
  plot_wordclouds(fl,
                  kset,
                  scale = scale,
                  max_words = max_words,
                  rot_per = rot_per,
                  brewer_pal = brewer_pal,
                  what_to_plot = "factors")
}

plot_loading_wordclouds <- function(fl,
                                    kset,
                                    scale = c(2, .1),
                                    max_words = 50,
                                    rot_per = .2,
                                    brewer_pal = "Blues") {
  plot_wordclouds(fl,
                  kset,
                  scale = scale,
                  max_words = max_words,
                  rot_per = rot_per,
                  brewer_pal = brewer_pal,
                  what_to_plot = "loadings")
}



plot_wordclouds <- function(fl,
                            kset,
                            scale,
                            max_words,
                            rot_per,
                            brewer_pal,
                            what_to_plot = c("factors", "loadings")) {
  what_to_plot <- match.arg(what_to_plot)
  X <- switch(what_to_plot, factors = fl$ldf$f, loadings = fl$ldf$l)

  words <- rownames(X)
  if (is.null(words)) {
    stop("plot_wordclouds requires that elements be named")
  }

  for (k in kset) {
    wordcloud::wordcloud(words = words,
                         freq = X[, k]^2,
                         scale = scale,
                         max.words = max_words,
                         random.order = FALSE,
                         rot.per = rot_per,
                         colors = RColorBrewer::brewer.pal(8, brewer_pal))
  }
}

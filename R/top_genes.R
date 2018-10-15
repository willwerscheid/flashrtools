get_top_genes <- function(fl,
                          kset = 1:fl$nfactors,
                          min_genes = 3,
                          max_genes = 20,
                          cutoff = 0.05,
                          where = c("loadings", "factors")) {
  where <- match.arg(where)

  ensembl <- biomaRt::useEnsembl(biomart = "ensembl",
                                 dataset = "hsapiens_gene_ensembl")

  ret <- list()
  for (k in kset) {
    vals <- switch(where,
                   loadings = fl$ldf$l[, k],
                   factors = fl$ldf$f[, k])

    top_vals <- sort(abs(vals), decreasing = TRUE)[1:max_genes]
    top_vals <- c(top_vals[1:min_genes],
                  top_vals[-(1:min_genes)][top_vals[-(1:min_genes)] > cutoff])
    gene_IDs <- names(top_vals)

    bm_info <- biomaRt::getBM(attributes = c('external_gene_name',
                                             'wikigene_description'),
                              filters = 'external_gene_name',
                              values = gene_IDs,
                              mart = ensembl)
    gene_desc <- bm_info$wikigene_description
    names(gene_desc) <- bm_info$external_gene_name

    ret[[k]] <- data.frame(geneID = gene_IDs,
                           val = vals[gene_IDs],
                           desc = gene_desc[gene_IDs])
  }

  return(ret)
}

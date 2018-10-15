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

find_genes_in_kset <- function(fl,
                               gene_IDs,
                               cutoff = 0.05,
                               where = c("loadings", "factors")) {
  where <- match.arg(where)

  vals <- switch(where,
                 loadings = fl$ldf$l,
                 factors = fl$ldf$f)
  ret <- list()

  for (gene_ID in gene_IDs) {
    if (!(gene_ID %in% row.names(vals))) {
      warning("Gene ", gene_ID, " not found.")
    } else {
      gene_vals <- vals[gene_ID, ]
      ret[[gene_ID]] <- which(gene_vals > cutoff)
    }
  }

  return(ret)
}

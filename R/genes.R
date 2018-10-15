# top_genes is a list of named vectors, as returned by get_top_elements
#
add_desc_to_top_genes <- function(top_genes) {
  ensembl <- biomaRt::useEnsembl(biomart = "ensembl",
                                 dataset = "hsapiens_gene_ensembl")

  return(lapply(top_genes, function(vals) {
    data.frame(gene_ID = names(vals),
               val = vals,
               desc = get_gene_descriptions(names(vals), ensembl),
               stringsAsFactors = FALSE)
  }))
}

get_gene_descriptions <- function(gene_IDs, ensembl) {
  bm_info <- biomaRt::getBM(attributes = c('external_gene_name',
                                           'wikigene_description'),
                            filters = 'external_gene_name',
                            values = gene_IDs,
                            mart = ensembl)
  gene_desc <- bm_info$wikigene_description
  names(gene_desc) <- bm_info$external_gene_name
  return(gene_desc[gene_IDs])
}

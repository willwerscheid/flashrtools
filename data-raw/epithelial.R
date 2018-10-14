# First I read in the data and convert it to a normal R matrix. Downloads
#   are here:
#     https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE102580

epithelial <- read.table("~/Downloads/GSE102580_filtered_normalized_counts_human.tsv",
                         header = TRUE, sep = '\t')
epi_names <- epithelial[, 1]
epithelial <- epithelial[, -1]
epithelial <- as.matrix(epithelial)
row.names(epithelial) <- epi_names
rm(epi_names)

# Next I use Seurat to do preprocessing.

library(Seurat)
epithelial <- CreateSeuratObject(epithelial,
                                 min.cells = 3, min.genes = 200)

# Note that filtering of cells with high mitochondrial percentage has
#   already been done.

GenePlot(epithelial, gene1 = "nUMI", gene2 = "nGene")
# Based on the above plot, I filter out any cells with nGene > 5000:
epithelial <- FilterCells(object = epithelial, subset.names = "nGene",
                          low.thresholds = -Inf, high.thresholds = 5000)

# The data has already been normalized, but I want it to be
#   log-normalized:
epithelial <- NormalizeData(epithelial,
                            normalization.method = "LogNormalize")

# I change the lower average expression cutoff, but other defaults for
#   finding highly variable genes seem appropriate:
epithelial <- FindVariableGenes(epithelial, x.low.cutoff = 0.0125)
length(x = epithelial@var.genes) # there are 1762 highly variable genes

# Regress out nUMI:
epithelial <- ScaleData(epithelial, vars.to.regress = "nUMI")

# Save the scaled data:
epithelial <- epithelial@scale.data[epithelial@var.genes, ]
devtools::use_data(epithelial, overwrite = TRUE)

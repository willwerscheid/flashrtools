# The data can be downloaded at:
#   https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE102580

epithelial <- read.table("~/Downloads/GSE102580_filtered_normalized_counts_human.tsv",
                         header = TRUE, sep = '\t')
epi.names <- epithelial[, 1]
epithelial <- epithelial[, -1]
epithelial <- as.matrix(epithelial)
row.names(epithelial) <- epi.names
rm(epi.names)
epithelial <- log(epithelial + 1)
devtools::use_data(epithelial)

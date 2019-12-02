library(Matrix)
library(data.table)

tblFiles <- c(
  tcells = "data/CountTable.Tcells.txt",
  bcells = "data/CountTable.Bcells.txt")

metaFiles <- c(
  tcells = "data/Meta.Data.Tcells.txt",
  bcells = "data/Meta.Data.Bcells.txt")

counts <- list()
for(x in names(tblFiles)) {
  a <- as.matrix(fread(tblFiles[x]), rownames = 1)
  a <- Matrix(a, sparse = TRUE)
  a <- t(a)
  counts[[x]] <- a
}

saveRDS(counts, "counts.rds")

metaInfo <- lapply(metaFiles, read.table)
for(x in names(metaInfo))
  metaInfo[[x]]$Subset <- factor(metaInfo[[x]]$Subset)

saveRDS(metaInfo, "metaInfo.rds")

totalCounts <- lapply(counts, rowSums)
saveRDS(totalCounts, "totalCounts.rds")

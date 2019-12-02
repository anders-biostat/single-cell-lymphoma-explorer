library(Matrix)
counts <- readRDS("counts.rds")
metaInfo <- readRDS("metaInfo.rds")
totalCounts <- readRDS("totalCounts.rds")

## assign clusters
TCELLTYPES <- c(
  "0" = "T[TOX]",
  "3" = "T[FH]",
  "2" = "T[REG]",
  "1" = "T[H]")
metaInfo$tcells$Subset <- TCELLTYPES[as.character(metaInfo$tcells$Subset)]
tcellColors <- c(
  `T[TOX]`=  "#D89000",
  `T[H]`  = "#00B0F6",
  `T[REG]` = "#00BF7D",
  `T[FH]` = "#F8766D")

sampleColors <- rev(RColorBrewer::brewer.pal(12, "Paired"))
names(sampleColors) <- c(
  "rLN1", "rLN2", "rLN3", "tFL1", "tFL2", "DLBCL1",
 "DLBCL2", "DLBCL3", "FL1", "FL2", "FL3", "FL4"
)


## set.seed(1)
## CELLNUM <- 200
## sampleTbl <- function(x) x[sample(dim(x)[1], CELLNUM),]
## sampleVec <- function(x) x[sample(length(x), CELLNUM)]

## metaInfo <- lapply(metaInfo, sampleTbl)
## counts <- lapply(counts, sampleTbl)
## totalCounts <- lapply(totalCounts, sampleVec)

correlationPlot <- function(x, genes) {
  if (is.null(genes) || length(genes) < 2)
    return(NULL)
  cortbl <- cor(as.matrix(x[, genes]), method = "spearman")
  ## remove any NAs due to zero variance
  diag(cortbl)[is.na(cortbl[1,])] <- 0
  cortbl[is.na(cortbl)] <- 0
  displayNumbers <- dim(cortbl)[1] < 15
  ## breaks <- quantile((unique(as.vector(cortbl))),
    ## seq(0, 1, length.out = 100))
  breaks <- (seq_len(50) / 50)^2
  breaks <- sort(c(-breaks,  breaks))
  pheatmap::pheatmap(
    cortbl,
    breaks = breaks,
    fontsize = 14,
    fontsize_number = 14,
    display_numbers = displayNumbers,
    na_col = "white")
}

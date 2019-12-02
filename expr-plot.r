source("helpers.r")

ggplotGeneExpr <- function(d, genes, groups, groupName, totals,
                           verticalXLab = FALSE) {
  zeroHeight <- .05
  xangle <- ifelse(verticalXLab, 90, 0)
  keep <- genes != "" & !duplicated(genes)
  if (!any(keep))
    return(NULL)
  genes <- genes[keep]
  d <- as.matrix(d[, genes, drop = FALSE])
  for(g in genes) {
    d[,g] <- scaleSeurat(d[,g], totals)
    d[,g] <- jitterZeros(d[,g], zeroHeight)
  }
  d <- as.data.frame(d)
  d$group <- groups
  d <- tidyr::gather(d, "gene", "count", -group)
  d$gene <- factor(d$gene, levels = genes)
  ggplot(data = d, aes(x = group, y = count)) +
    geom_rect(aes(xmin = -Inf, xmax = +Inf,
      ymin = -zeroHeight, ymax = zeroHeight),
      fill="grey", alpha = .4) +
    geom_violin(size = 1.5, fill = NA) +
    geom_jitter(width = .3, size = .5) +
    ggplot2::facet_wrap("gene", ncol = 1, scales = "free") +
    theme_bw() +
    xlab(groupName) +
    ylab("norm. count") +
    scale_x_discrete(labels = parse(text = levels(factor(d$group)))) +
    theme(text = element_text(size = TEXTSIZE),
      axis.text.x = element_text(angle = xangle, hjust = .5, vjust = .5))
}

jitterZeros <- function(x, height) {
  set.seed(1)
  x[x==0] <- jitter(x[x==0], amount = height)
  x
}

## get a table with counts and labels
swarmPlot <- function(d, genes, cols, totals) {
  zeroHeight <- .2
  keep <- genes != "" & !duplicated(genes)
  cols <- cols[keep]
  genes <- genes[keep]
  d <- d[, genes, drop = FALSE]
  for(g in genes) {
    d[,g] <- scaleSeurat(d[,g], totals)
    d[, g] <- jitterZeros(d[, g], zeroHeight)
  }
  d <- as.data.frame(as.matrix(d))
  d <- tidyr::gather(d, "gene", "count")
  d$gene <- factor(d$gene, levels = genes)
  ggplot(data = d, aes(x = gene, y = count)) +
    geom_rect(aes(xmin = -Inf, xmax = +Inf,
      ymin = -zeroHeight, ymax = zeroHeight),
      fill="grey", alpha = .4) +
    geom_jitter(width = .3, size = .5)  +
    ylab("norm. count") +
    geom_violin(aes(color = gene), size = 1.5, fill = NA) +
    scale_colour_manual(labels = genes, values = cols) +
    theme_bw() +
    theme(text = element_text(size = TEXTSIZE))
}

source("helpers.r")

plotUmap <- function(d, groups, groupName, cellSize = 1,
                     groupColours = NULL) {
  d$id <- rownames(d)
  d$groups <- groups
  if (!is.null(groupColours))
    d$groups <- factor(groups, levels = names(groupColours))
  q <- ggplot(data = d, aes_string(x = "UMAP1", y = "UMAP2")) +
    geom_point(aes_string(colour = "groups", customdata = "id"),
      size = cellSize) +
    guides(colour = guide_legend(title = groupName,
      override.aes = list(size = 5))) +
    theme_bw() +
    theme(text = element_text(size = .5*TEXTSIZE))
  if (!is.null(groupColours))
    q <- q + scale_colour_manual(values = groupColours)
  q
}

plotUmapExpr <- function(d, exprValues, total, colourGenes, cellSize) {
  keep <- colourGenes != "" & !duplicated(colourGenes)
  colourGenes <- colourGenes[keep]
  exprValues <- getCountsForColours(exprValues, colourGenes)
  exprValues <- lapply(exprValues, scaleSeurat, total)
  ggplotUmapExpr(d, exprValues, cellSize)
}

ggplotUmapExpr <- function(d, exprValues,  cellSize) {
  colours <- do.call(values2colour, exprValues)
  d$id <- rownames(d)
  ggplot(data = d, aes_string(x = "UMAP1", y = "UMAP2")) +
    geom_point(aes_string(customdata = "id"),
      colour = colours, size = cellSize) +
    scale_colour_viridis_c() +
    theme_bw() +
    theme(panel.background = element_rect(fill = "grey30"),
      panel.grid = element_line(color="grey85", size = 0.5),
      text = element_text(size = .5*TEXTSIZE))
}

## just map every interval to [0,1] and then to the colour channel
values2colour <- function(red = 0, green = 0, blue = 0) {
  vals <- list(red = red, green = green, blue = blue)
  vals <- lapply(vals, function(x) {
    if (all(x == 0)) return(0)
    x / max(x)})
  vals <- do.call(cbind, vals)
  do.call(rgb, data.frame(vals))
}


getCountsForColours <- function(d, genes) {
  lapply(genes,
    function(g) {
      if (g == "") return(NULL)
      d[,g]
    })
}

TEXTSIZE <- 24

scaleSeurat <- function(x, total) {
  log10(1 + 10^4 * x / total)
}

plotmath2html <- function(x) {
  sub("\\[(.+?)\\]", "<sub>\\1</sub>", x)
}

## takes first word before "_"
getSampleFromCellName <- function(cellnames) {
  sub("_.+", "", cellnames)
}

## B cells
bcellExprPlotTab <- tabPanel("Gene expression UMAP",
  column(3,
    h4("Genes for B cells"),
    selectizeInput("umapBcellsGeneRed",
      span("RED", style='color:red'),
      choices = NULL,
      options = list(placeholder = "gene name...")),
    selectizeInput("umapBcellsGeneGreen",
      span("GREEN", style='color:green'),
      choices = NULL,
      options = list(placeholder = "gene name...")),
    selectizeInput("umapBcellsGeneBlue",
      span("BLUE", style='color:blue'),
      choices = NULL,
      options = list(placeholder = "gene name...")),
    shiny::selectInput("geneExprBcellsPlotType",
      "Plot type",
      choices = c("all", "by sample", "by cluster"))
  ),
  column(9, shiny::plotOutput("geneExprBcells")))

bcellCorrelationTab <- tabPanel("Gene-gene correlation",
  column(3,
    selectizeInput("correlationPlotGenesBcells",
      "Gene list",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "gene name..."))),
  column(9,
    h4("Spearman's correlation"),
    plotOutput("correlationPlotBcells"))
)

## T cells
tcellExprPlotTab <- tabPanel("Gene expression UMAP",
  column(3,
    h4("Genes for T cells"),
    selectizeInput("umapTcellsGeneRed",
      span("RED", style='color:red'),
      choices = NULL,
      options = list(placeholder = "gene name...")),
    selectizeInput("umapTcellsGeneGreen",
      span("GREEN", style='color:green'),
      choices = NULL,
      options = list(placeholder = "gene name...")),
    selectizeInput("umapTcellsGeneBlue",
      span("BLUE", style='color:blue'),
      choices = NULL,
      options = list(placeholder = "gene name...")),
    shiny::selectInput("geneExprTcellsPlotType",
      "Plot type",
      choices = c("all", "by sample", "by cluster"))
  ),
  column(9, shiny::plotOutput("geneExprTcells")))

tcellCorrelationTab <- tabPanel("Gene-gene correlation",
  column(3,
    selectizeInput("correlationPlotGenesTcells",
      "Gene list",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "gene name..."))),
  column(9,
    h4("Spearman's correlation"),
    plotOutput("correlationPlotTcells"))
)

library(plotly)
library(shiny)

set.seed(1)
source("loadTbl.r", local = TRUE)
source("corr-plot.r", local = TRUE)
source("umap-plot.r", local = TRUE)
source("expr-plot.r", local = TRUE)
source("ui-helpers.r", local = TRUE)

ui <- fluidPage(
  ## tags$style(".selectize-dropdown {width: auto !important;}"),
  tags$style("body {padding-bottom: 200px}"),
  titlePanel(
      h1("Single Cell Lymphoma Explorer", align = "center"),
      windowTitle = "Single Cell Lymphoma Explorer"),
  fluidRow(
    column(6,
      div(
        h2("B cells", style = "text-align:center"),
        plotlyOutput("umapBcells", width = "600px", height = "500px"),
        br(),
        radioButtons("umapBcellsType", label = "Colour by",
          choices = c("cluster", "sample", "expression"),
          inline = TRUE),
      style = "width:600px; margin: 0 auto;"),
      br(),
      tabsetPanel(
        bcellExprPlotTab,
        bcellCorrelationTab),
    ),
    column(6,
      div(
        h2("T cells", style = "text-align:center"),
        plotlyOutput("umapTcells", width = "600px", height = "500px"),
        br(),
        radioButtons("umapTcellsType", label = "Colour by",
          choices = c("cluster", "sample", "expression"),
          inline = TRUE),
      style = "width:600px; margin: 0 auto;"),
      br(),
      tabsetPanel(
        tcellExprPlotTab,
        tcellCorrelationTab)
    )
  )
)

server <- function(input, output, session) {
  ## create selectors
  bcellGeneChoices <- colnames(counts[["bcells"]])
  tcellGeneChoices <- colnames(counts[["tcells"]])
  for(color in c("Blue", "Red", "Green")) {
    updateSelectizeInput(session,
      paste0("umapBcellsGene", color),
      selected = FALSE,
      choices = c("", bcellGeneChoices), server = TRUE)
    updateSelectizeInput(session,
      paste0("umapTcellsGene", color),
      selected = FALSE,
      choices = c("", tcellGeneChoices), server = TRUE)
  }
  for(cellType in c("Bcells", "Tcells")) {
    updateSelectizeInput(session,
      paste0("correlationPlotGenes", cellType),
      selected = FALSE,
      choices = c("", tcellGeneChoices), server = TRUE)
  }

  ## B cells
  output[["umapBcells"]] <- renderPlotly({
    dat <- metaInfo[["bcells"]]
    dat$Subset <- plotmath2html(dat$Subset)
    p <- switch(input$umapBcellsType,
      "cluster"    = plotUmap(dat, dat$Subset, "Cluster", .5),
      "sample"     = plotUmap(dat, getSampleFromCellName(rownames(dat)),
        "Sample", .5, sampleColors),
      "expression" = plotUmapExpr(
        metaInfo[["bcells"]],
        counts[["bcells"]],
        totalCounts[["bcells"]],
        colourGenes = list(
          red   = input$umapBcellsGeneRed,
          green = input$umapBcellsGeneGreen,
          blue  = input$umapBcellsGeneBlue
        ),
        cellSize = .5))
    ply <- ggplotly(p, source = "umapBcells",
      width = 600, height = 500) %>%
      layout(dragmode = "select") %>%
      event_register("plotly_selecting")
    ply
  })
  bcellGenes <- reactive({c(
    input$umapBcellsGeneRed,
    input$umapBcellsGeneGreen,
    input$umapBcellsGeneBlue)})
  bcellExprHeight <- reactive({
    switch(input$geneExprBcellsPlotType,
      "by cluster" = ,
      "by sample" = max(350, sum(unique(bcellGenes()) != "") * 350),
      "all" = 350)
  })
  output[["geneExprBcells"]] <- renderPlot({
    genes <- bcellGenes()
    if (all(genes == ""))
      return(NULL)
    i <- event_data("plotly_selected", source = "umapBcells")
    cells <- rownames(counts[["bcells"]])
    if (!is.null(i)) {
      cells <- rownames(counts[["bcells"]]) %in% i$customdata
    }
    cols <- c("red", "green", "blue")
    tbl <- counts[["bcells"]][cells,]
    totals <- totalCounts[["bcells"]][cells]
    switch(input$geneExprBcellsPlotType,
      "all"       = swarmPlot(tbl, genes, cols, totals),
      "by sample" = ggplotGeneExpr(
        tbl, genes,
        factor(getSampleFromCellName(rownames(tbl)), names(sampleColors)),
        "sample",
        totals = totals, verticalXLab = TRUE),
      "by cluster" = ggplotGeneExpr(
        tbl, genes,
        metaInfo[["bcells"]][cells, "Subset"],
        "cluster",
        totals = totals),
      )
  },
  height = function() {bcellExprHeight()})

  bcellCorrHeight <- reactive(
    {200 + 20 * length(input$correlationPlotGenesBcells)})
  output[["correlationPlotBcells"]] <- renderPlot({
    genes <- input$correlationPlotGenesBcells
    correlationPlot(counts[["bcells"]], genes)
  }, height = function() {bcellCorrHeight()})

  ## T cells
  output[["umapTcells"]] <- renderPlotly({
    dat <- metaInfo[["tcells"]]
    dat$Subset <- plotmath2html(dat$Subset)
    p <- switch(input$umapTcellsType,
      "cluster"    = plotUmap(dat, dat$Subset, "Cluster", .5),
      "sample"     = plotUmap(dat, getSampleFromCellName(rownames(dat)),
        "Sample", .5, sampleColors),
      "expression" = plotUmapExpr(
        metaInfo[["tcells"]],
        counts[["tcells"]],
        totalCounts[["tcells"]],
        colourGenes = list(
          red   = input$umapTcellsGeneRed,
          green = input$umapTcellsGeneGreen,
          blue  = input$umapTcellsGeneBlue
        ),
        cellSize = .5))
    ply <- ggplotly(p, source = "umapTcells",
      width = 600, height = 500) %>%
      layout(dragmode = "select") %>%
      event_register("plotly_selecting")
    ply
  })
  tcellGenes <- reactive({c(
    input$umapTcellsGeneRed,
    input$umapTcellsGeneGreen,
    input$umapTcellsGeneBlue)})
  tcellExprHeight <- reactive({
    switch(input$geneExprTcellsPlotType,
      "by cluster" = ,
      "by sample" = max(350, sum(unique(tcellGenes()) != "") * 350),
      "all" = 350)
  })
  output[["geneExprTcells"]] <- renderPlot({
    genes <- tcellGenes()
    if (all(genes == ""))
      return(NULL)
    i <- event_data("plotly_selected", source = "umapTcells")
    cells <- rownames(counts[["tcells"]])
    if (!is.null(i)) {
      cells <- rownames(counts[["tcells"]]) %in% i$customdata
    }
    cols <- c("red", "green", "blue")
    tbl <- counts[["tcells"]][cells,]
    totals <- totalCounts[["tcells"]][cells]
    switch(input$geneExprTcellsPlotType,
      "all"       = swarmPlot(tbl, genes, cols, totals),
      "by sample" = ggplotGeneExpr(
        tbl, genes,
        factor(getSampleFromCellName(rownames(tbl)), names(sampleColors)),
        "sample",
        totals = totals, verticalXLab = TRUE),
      "by cluster" = ggplotGeneExpr(
        tbl, genes,
        metaInfo[["tcells"]][cells, "Subset"],
        "cluster",
        totals = totals),
      )
  },
  height = function() {tcellExprHeight()})
  tcellCorrHeight <- reactive(
  {200 + 20 * length(input$correlationPlotGenesTcells)})
  output[["correlationPlotTcells"]] <- renderPlot({
    genes <- input$correlationPlotGenesTcells
    correlationPlot(counts[["tcells"]], genes)
  }, height = function() {tcellCorrHeight()})

}

shinyApp(ui, server)

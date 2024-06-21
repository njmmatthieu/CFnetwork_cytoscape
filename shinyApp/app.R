library(cyjShiny)
library(graph)
library(jsonlite)
library(htmlwidgets)
library(shiny)

graph.json.filename <- "CF_network_kegg_diff_pathways_with_CFTR_interactors_20240621.cyjs"
graph <- fromJSON(graph.json.filename,
                  flatten = TRUE)
graph.nodes.df <- graph$elements$nodes
graphAsJSON <- readAndStandardizeJSONNetworkFile(graph.json.filename)
style.json.filename <- "CF_network_style.json"

CFTR_interactors <- graph.nodes.df[which(graph.nodes.df$data.CFTR_interactor==T),
                                  "data.name"]

pathways_names <- c("C-type lectin receptor signaling pathway",
                    "Cytokine-cytokine receptor interaction",
                    "Cytosolic DNA-sensing pathway",
                    "Estrogen signaling pathway",
                    "IL-17 signaling pathway",
                    "NF-kappa B signaling pathway",
                    "NOD-like receptor signaling pathway",
                    "Osteoclast differentiation",
                    "Regulation of actin cytoskeleton",
                    "RIG-I-like receptor signaling pathway",
                    "T cell receptor signaling pathway",
                    "Th17 cell differentiation",
                    "TNF signaling pathway",
                    "Toll-like receptor signaling pathway",
                    "Viral protein interaction with cytokine and cytokine receptor")

pathways_colnames <- paste("data.", 
                           gsub("[[:punct:]]+|[[:space:]]", 
                                "_", 
                                pathways_names), 
                           sep = "")

source_nodes <- c("TRADD",
                  "PRKACA",
                  "SYK",
                  "CSNK2A1",
                  "SRC",
                  "PLCB1",
                  "PLCB3",
                  "EZR")

# SET INPUT OPTIONS ----
colorByList <- c("", 
                 "Subgroup1 logFC"="logFC_subgroup1.json",
                 "Subgroup2 logFC "="logFC_subgroup2.json",
                 "Betweenness Centrality Score"="BC_score.json",
                 "Rauniyar logFC"="logFC_Rauniyar.json")


# UI ----
ui <-  shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css"),
    tags$style("#cyjShiny{height:95vh !important;}")),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectName", 
                  "Select Node by Gene Name:", 
                  choices = c("", sort(graph.nodes.df$data.name))),
      selectInput("selectPathway", 
                  "Select Pathway:", 
                  choices = c("", pathways_names)),
      selectInput("selectSourceNodes", 
                  "Select Source Node",
                  choices = c("", sort(source_nodes), "all")),
      selectInput("loadStyleFile", 
                  "Color By: ", 
                  choices=colorByList),
      actionButton("fit", "Fit Graph"),
      actionButton("fitSelected", "Fit Selected"),
      actionButton("sfn", "Select First Neighbor"),
      actionButton("clearSelection", "Unselect Nodes"),
      HTML("<br>"),
      htmlOutput("selectedNodesDisplay"),
      width=2
    ),
    mainPanel(
      cyjShinyOutput('cyjShiny'),
      width=10
    )
  ) # sidebarLayout
))

# SERVER ----
server <- function(input, output, session) {
  # Event observers 
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80)
  })
  
  observeEvent(input$doLayout, ignoreInit=TRUE,{
      strategy <- input$doLayout
      doLayout(session, strategy)
      session$sendCustomMessage(type="doLayout", message=list(input$doLayout))
  })
  
  observeEvent(input$selectName, ignoreInit=TRUE,{
    GeneNames <- input$selectName
    gene_id <- graph.nodes.df[which(graph.nodes.df$data.shared_name %in% GeneNames), 
                                    "data.id"]
    session$sendCustomMessage(type="selectNodes", 
                              message=list(gene_id))
  })
  
  observeEvent(input$selectPathway, ignoreInit=TRUE,{
    PathwayNames <- input$selectPathway
    PathwayIds <- paste("data.", 
                        gsub("[[:punct:]]+|[[:space:]]",
                             "_", 
                             PathwayNames), 
                        sep = "")
    gene_id <- graph.nodes.df[which(graph.nodes.df[,PathwayIds]==1), 
                              "data.id"]
    selectNodes(session, gene_id)
  })
  
  observeEvent(input$selectSourceNodes, ignoreInit=TRUE,{
    SourceName <- input$selectSourceNodes
    if (SourceName=="all"){
      gene_id <- graph.nodes.df[which(graph.nodes.df[,"data.name"] %in% source_nodes), 
                                "data.id"]
      selectNodes(session, gene_id)
    } else {
      gene_id <- graph.nodes.df[which(graph.nodes.df$data.name==SourceName), 
                                "data.id"]
      selectNodes(session, gene_id)
    }
  })
  
  observeEvent(input$loadStyleFile, ignoreInit=TRUE, {
    if(input$loadStyleFile != "") {
      print(input$loadStyleFile)
      # styleFile = colorByList[[input$loadStyleFile]]
      # print(styleFile)
      print(file.exists(input$loadStyleFile))
      loadStyleFile(input$loadStyleFile)
      # output$cyjShiny <- renderCyjShiny({
      #   cyjShiny(graphAsJSON, 
      #            layoutName="preset", 
      #            styleFile=styleFile)
      # })
    }
  })
  
  observeEvent(input$fitSelected, ignoreInit=TRUE,{
    fitSelected(session, 100)
  })
  
  observeEvent(input$sfn, ignoreInit=TRUE,{
    session$sendCustomMessage(type="sfn", message=list())
  })
  
  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    getSelectedNodes(session)
  })
  
  observeEvent(input$clearSelection, ignoreInit=TRUE, {
    session$sendCustomMessage(type="clearSelection", message=list())
  })

  
  observeEvent(input$selectedNodes, {
    newNodes <- input$selectedNodes;
    output$selectedNodesDisplay <- renderText({
      paste(newNodes)
    })
  })
  
  # Output variables 
  output$value <- renderPrint({ input$action })
  
  output$cyjShiny <- renderCyjShiny({
    cyjShiny(graphAsJSON, 
             layoutName="preset", 
             styleFile=style.json.filename)
  })
} 

# RUN SHINY APP ----
shinyApp(ui = ui, server = server)


library(cyjShiny)
library(graph)
library(jsonlite)
library(htmlwidgets)
library(shiny)

graph.json.filename <- "CF_network_kegg_diff_pathways_with_CFTR_interactors_and_CFTR_without_unconnected_components.cyjs"
graph <- fromJSON(graph.json.filename,
                  flatten = TRUE)
graph.nodes.df <- graph$elements$nodes
graphAsJSON <- readAndStandardizeJSONNetworkFile(graph.json.filename)
style.json.filename <- "CF_network_style.json"
# 
# CF_PPI_network.lcc.node_type.nodes <- 
#   read.table(file = "CF_network_kegg_diff_pathways_with_CFTR_interactors_direct_tagged_nodes_df.txt",
#              sep = "\t",
#              header = T,
#              check.names = F)

# UI ----
ui <-  shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css"),
    tags$style("#cyjShiny{height:95vh !important;}")),
  
  sidebarLayout(
    sidebarPanel(
      # selectInput("doLayout", "Select Layout:",
      #             choices=c("",
      #                       "cose",
      #                       "cola",
      #                       "circle",
      #                       "concentric",
      #                       "breadthfirst",
      #                       "grid",
      #                       "random",
      #                       "preset",
      #                       "fcose")),
      
      selectInput("selectName", 
                  "Select Node by Gene Name:", 
                  choices = c("", sort(graph.nodes.df$data.name))),
      actionButton("fit", "Fit Graph"),
      actionButton("fitSelected", "Fit Selected"),
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
  
  observeEvent(input$fitSelected, ignoreInit=TRUE,{
    fitSelected(session, 100)
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


library(shiny)
library(data.table)
library(igraph)
library(dplyr)
library(threejs)
library(ggplot2)

dt.crimes <- as.data.table(read.csv("Crimes.2017.csv"))
ui <-    fluidPage(
  tabsetPanel(
    tabPanel("Network of Crimes", 
             titlePanel('Network Exploration'),
             sidebarLayout(
               sidebarPanel(
                 h2("Showing bipartite projection"),
                 selectInput(inputId = 'location', label = 'Select Location Varibale', choices = c('District', 'Ward'), selected = 'District'),
                 selectInput(inputId = 'edge', label = 'Choose Edges', choices = c('Crime Type', 'Location'), selected = 'Crime Type'),
                 #sliderInput(inputId = 'degree', label = 'Select Degree Range', min = 0, max = 6000, value = 10),
                 #dateRangeInput(inputId = 'daterange', label = 'Choose a Date Range', start = '2017-01-01', end = '2017-12-31', startview = 'month', separator = ' to '),
                 selectInput(inputId = 'centrality', label = 'Select Centrality Measure', choices = c('Degree', 'Closeness Centrality', 'Betweenness Centrality', 'Eigenvector Centrality'), selected = 'Degree')),
               mainPanel(   
                 plotOutput('graph'),
                 plotOutput('degreedist'),
                 #      DT::DTOutput("networksummary"),
                 tableOutput("topcentralities"),
                 tableOutput("centralitystats")
               ))
    ),
    tabPanel("Network of Wards")
  )) 




# ---- Network Exploration ----

server <- function(input, output, session) {
  
  ### Prepare data to be used
  dt.crimes.filtered.location <- dt.crimes[, list(Primary.Type, Ward, District, Date.Time)]
  dt.crimes.filtered.location$Date.Time <- as.Date(dt.crimes.filtered.location$Date.Time)
  
  ## Add column for projection with Locations as nodes
  dt.crimes.filtered.location <- dt.crimes.filtered.location %>% mutate(Type.Time = paste(Primary.Type, Date.Time))
  dt.crimes.filtered.location <- dt.crimes.filtered.location[duplicated(Type.Time), ]
  
  ## Add column for projection with Primary.Types as nodes and Wards as edges
  dt.crimes.filtered.crimes <- dt.crimes.filtered.location %>% mutate(Ward.Time = paste(Ward, Date.Time))
  dt.crimes.filtered.crimes <- dt.crimes.filtered.crimes[duplicated(Ward.Time), ]
  
  ## Add column for projection with Primary.Types as nodes and Districts as edges
  dt.crimes.filtered.crimes.districts <- dt.crimes.filtered.location %>% mutate(District.Time = paste(District, Date.Time))
  dt.crimes.filtered.crimes.districts <- dt.crimes.filtered.crimes.districts[duplicated(District.Time), ]
  
  mergekey <- reactive({
    
    if (input$edge == 'Crime Type') {
      mergekey <- 'Type.Time'
    } else if (input$location == 'Ward' & input$edge == 'Location') {
      mergekey <- 'Ward.Time'
    } else if (input$location == 'District' & input$edge == 'Location') {
      mergekey <- 'District.Time'
    }
  })
  
  edgelist <- reactive({
    edgelist <- merge(dt.crimes.filtered.location, dt.crimes.filtered.location, by = mergekey(), allow.cartesian = TRUE)
    edgelist <- edgelist[!duplicated(edgelist)]
    if (input$location == 'Ward' & input$edge == 'Crime Type') {
      edgelist.wards <- edgelist[, list(Type.Time, Ward.x, Ward.y)][Ward.x != Ward.y, ]
      edgelist.wards <- edgelist.wards[, weight := .N, by = list(Ward.x, Ward.y)][!duplicated(edgelist.wards)]
      edgelist.wards$min <- with(edgelist.wards, pmin(Ward.x, Ward.y))
      edgelist.wards$max <- with(edgelist.wards, pmax(Ward.x, Ward.y))
      edgelist.wards <- edgelist.wards %>% mutate(Type.Time.Edge = paste(Type.Time, min, max))
      edgelist.wards <- edgelist.wards[!duplicated(Type.Time.Edge)][, list(Ward.x, Ward.y, weight, Type.Time)]
      edgelist.wards <- as.matrix(edgelist.wards[, list(Ward.x, Ward.y, weight)])
    } else if (input$location == 'District' & input$edge == 'Crime Type') {
      edgelist.districts <- edgelist[, list(Type.Time, District.x, District.y)][District.x != District.y, ][, weight := .N, by = list(District.x, District.y)]
      edgelist.districts <- edgelist.districts[!duplicated(edgelist.districts)]
      edgelist.districts$min <- with(edgelist.districts, pmin(District.x, District.y))
      edgelist.districts$max <- with(edgelist.districts, pmax(District.x, District.y))
      edgelist.districts <- edgelist.districts %>% mutate(Type.Time.Edge = paste(Type.Time, min, max))
      edgelist.districts <- edgelist.districts[!duplicated(Type.Time.Edge), list(District.x, District.y, weight, Type.Time)]
      edgelist.districts <- as.matrix(edgelist.districts[, list(District.x, District.y, weight)])
    } else if (input$location == 'Ward' & input$edge == 'Location') {
      edgelist.primary.wards <- merge(dt.crimes.filtered.crimes, dt.crimes.filtered.crimes, by = mergekey(), allow.cartesian = TRUE)
      edgelist.primary.wards <- edgelist.primary.wards[!duplicated(edgelist.primary.wards)]
      edgelist.primary.wards <- edgelist.primary.wards[, list(Ward.Time, Primary.Type.x, Primary.Type.y)][Primary.Type.x != Primary.Type.y, ][, weight := .N, by = list(Primary.Type.x, Primary.Type.y, Ward.Time)]
      edgelist.primary.wards <- edgelist.primary.wards[!duplicated(edgelist.primary.wards)]
      edgelist.primary.wards <- edgelist.primary.wards[!duplicated(lapply(as.data.frame(t(edgelist.primary.wards), stringsAsFactors=FALSE), sort)),][, list(Primary.Type.x, Primary.Type.y, weight, Ward.Time)]
      edgelist.primary.wards <- as.matrix(edgelist.primary.wards[, list(Primary.Type.x, Primary.Type.y, weight)])
    } else if (input$location == 'District' & input$edge == 'Location') {
      edgelist.primary.districts <- merge(dt.crimes.filtered.crimes.districts, dt.crimes.filtered.crimes.districts, by = mergekey(), allow.cartesian = TRUE)
      edgelist.primary.districts <- edgelist.primary.districts[!duplicated(edgelist.primary.districts)]
      edgelist.primary.districts <- edgelist.primary.districts[, list(District.Time, Primary.Type.x, Primary.Type.y)][Primary.Type.x != Primary.Type.y, ][, weight := .N, by = list(Primary.Type.x, Primary.Type.y, District.Time)]
      edgelist.primary.districts <- edgelist.primary.districts[!duplicated(edgelist.primary.districts)]
      edgelist.primary.districts <- edgelist.primary.districts[!duplicated(lapply(as.data.frame(t(edgelist.primary.districts), stringsAsFactors=FALSE), sort)),][, list(Primary.Type.x, Primary.Type.y, weight, District.Time)]
      edgelist.primary.districts <- as.matrix(edgelist.primary.districts[, list(Primary.Type.x, Primary.Type.y, weight)])
    }
  })
  
  graphObject <- reactive({
    
    g.graph <- graph.edgelist(edgelist()[, 1:2], directed = FALSE)
    E(g.graph)$weight <- as.numeric(edgelist()[, 3])
    V(g.graph)$degree <- igraph::degree(g.graph)
    V(g.graph)$closeness <- igraph::closeness(g.graph)
    V(g.graph)$betweenness <- igraph::betweenness(g.graph)
    V(g.graph)$evcent <- igraph::evcent(g.graph)$vector
    
    
  })
  
  tableObject <- reactive({
    dt.g.graph <- data.table::data.table(igraph::get.data.frame(graphObject(), "vertices"))
    dt.g.graph <- dt.g.graph[, name := rownames(dt.g.graph)]
    dt.g.graph.order <- data.frame(degreename = head(dt.g.graph[order(-degree)], 20)$name,
                                   degree = head(dt.g.graph[order(-degree)], 20)$degree,
                                   closenessname = head(dt.g.graph[order(-closeness)], 20)$name,
                                   closeness = head(dt.g.graph[order(-closeness)], 20)$closeness,
                                   betweennessname = head(dt.g.graph[order(-betweenness)], 20)$name,
                                   betweenness = head(dt.g.graph[order(-betweenness)], 20)$betweenness,
                                   evcentname = head(dt.g.graph[order(-evcent)], 20)$name,
                                   evcent = head(dt.g.graph[order(-evcent)], 20)$evcent)
    })
 
 tableObject <- reactive({ 
   
   dt.g.graph <- data.table::data.table(igraph::get.data.frame(graphObject(), "vertices"))
   
   if (input$centrality == "Betweenness Centrality") {
   dt.g.graph.betwenness <- data.frame(Minimum = min(dt.g.graph$betweenness),
                                             Maximum = max(dt.g.graph$betweenness),
                                            Standard Deviation =  sd(dt.g.graph$betweenness)
                                            Mean = mean(dt.g.graph$betweenness))
   
   } else if (input$centrality == "Degree Centrality") {
     dt.g.graph.betwenness <- data.frame(Minimum = min(dt.g.graph$deegre),
                                         Maximum = max(dt.g.graph$deegre),
                                         Standard Deviation = sd(dt.g.graph$deegre),
                                         Mean = mean(dt.g.graph$deegre))
 
 } else if (input$centrality == "Closeness Centrality") {
   dt.g.graph.betwenness <- data.frame(Minimum = min(dt.g.graph$closeness),
                                       Maximum = max(dt.g.graph$closeness),
                                       Standard Deviation = sd(dt.g.graph$closeness),
                                       Mean = mean(dt.g.graph$closeness))
   
  } else if (input$centrality == "Eigenvector Centrality") {
    dt.g.graph.betwenness <- data.frame(Minimum = min(dt.g.graph$evcent),
                                        Maximum = max(dt.g.graph$evcent),
                                        Standard Deviation = sd(dt.g.graph$evcent),
                                        Mean = mean(dt.g.graph$evcent))
}
 
 })
    
  output$graph <- renderPlot({
    igraph::plot.igraph(graphObject(), vertex.size = 3, vertex.label = NA)
  })
  
  
  output$degreedist <- renderPlot({
    ggplot2::qplot(degree(graphObject()), geom = 'histogram', binwidth = 1)
  })
  
  output$topcentralities <- renderTable({
    tableObject()
  }) 
  
  output$centralitystats <- renderTable({
    tableObject()
  })  
  
}

shinyApp(ui, server)

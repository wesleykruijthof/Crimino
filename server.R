function(input, output, session) {
  theme_set(theme_classic() + theme(text=element_text(family="Raleway")))
  
  dt.crimes.filtered <- dt.crimes2[, list(Primary.Type, Ward, District, Date.Time)]
  dt.crimes.filtered$Date.Time <- as.Date(dt.crimes.filtered$Date.Time)
  
  dt.freq.crimes <- dt.crimes[, .N, by = Primary.Type][order(-N)]
  
  # ---- Reactive Inputs ----
  output$race.white.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Race.White_pct', label = 'Percentage "White"', min = 0, max = 1, value = dt.ward[dt.ward$Ward == input$ward.input, "Race.White_pct"])
    )
  })
  output$race.black.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Race.Black_pct', label = 'Percentage "Black"', min = 0, max = 1, value = dt.ward[dt.ward$Ward == input$ward.input, "Race.Black_pct"])
    )
  })
  output$race.asian.input <- renderUI({
    tagList(
      shinyjs::disabled(sliderInput(inputId = 'Race.Asian_pct', label = 'Percentage "Asian"', min = 0, max = 1, value = (1 - input$Race.White_pct - input$Race.Black_pct)))
    )
  })
  output$ethnicity.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Ethnicity.Hispanic_pct', label = 'Percentage "Hispanic Ethnicity"', min = 0, max = 1, value = dt.ward[dt.ward$Ward == input$ward.input, "Ethnicity.Hispanic_pct"])
    )
  })
  output$police.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Police_Stations', label = 'Number of Police Stations in a Ward', min = min(dt.ward$Police_Stations), max = max(dt.ward$Police_Stations), value = dt.ward[dt.ward$Ward == input$ward.input, "Police_Stations"])
    )
  })
  output$income.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Income', label = 'Average Income Level', min = min(dt.ward$Income), max = max(dt.ward$Income), value = dt.ward[dt.ward$Ward == input$ward.input, "Income"])
    )
  })
  output$service.calls.input <- renderUI({
    tagList(
      sliderInput(inputId = 'Abandoned_Vehicles', label = 'Number of Abandonded Vehicle Requests', min = min(dt.ward$Abandoned_Vehicles), max = max(dt.ward$Abandoned_Vehicles), value = dt.ward[dt.ward$Ward == input$ward.input, "Abandoned_Vehicles"]),
      sliderInput(inputId = 'Garbage_Carts', label = 'Number of Garbage Cart Requests', min = min(dt.ward$Garbage_Carts), max = max(dt.ward$Garbage_Carts), value = dt.ward[dt.ward$Ward == input$ward.input, "Garbage_Carts"]),
      sliderInput(inputId = 'Rodent_Baiting', label = 'Number of Rodent Baiting Requests', min = min(dt.ward$Rodent_Baiting), max = max(dt.ward$Rodent_Baiting), value = dt.ward[dt.ward$Ward == input$ward.input, "Rodent_Baiting"]),
      sliderInput(inputId = 'Sanitation_Code_Complaints', label = 'Number of Sanitation Code Complaints', min = min(dt.ward$Sanitation_Code_Complaints), max = max(dt.ward$Sanitation_Code_Complaints), value = dt.ward[dt.ward$Ward == input$ward.input, "Sanitation_Code_Complaints"]),
      sliderInput(inputId = 'Graffiti_Removal', label = 'Number of Gaffiti Removal Requests', min = min(dt.ward$Graffiti_Removal), max = max(dt.ward$Graffiti_Removal), value = dt.ward[dt.ward$Ward == input$ward.input, "Graffiti_Removal"]),
      sliderInput(inputId = 'Abandoned_Buildings', label = 'Number of Abandonded Building Requests', min = min(dt.ward$Abandoned_Buildings), max = max(dt.ward$Abandoned_Buildings), value = dt.ward[dt.ward$Ward == input$ward.input, "Abandoned_Buildings"])
    )
  })
  
  
  # ---- Data Explorer ----
  # -- Summary Statistics Tab
  output$tb.descriptives <- renderTable({
    dt.descriptives <- data.frame(Category = c("Total nº of observations", "Unique crime types", "Unique crime groups - Primary.Type", "Total nº unique locations", "Arrests"), Statistics = c(nrow(dt.crimes), length(unique(dt.crimes$Description)), length(unique(dt.crimes$Primary.Type)), length(unique(dt.crimes$Location)), nrow(dt.crimes[Arrest == "Yes", ])))
  })
  
  output$hist.ward <- renderPlot({
    ggplot(dt.crimes, aes(x = Ward)) + geom_histogram() + ggtitle("Frequency of crimes per ward")
  })
  
  output$hist.Primary.Type <- renderPlot({
    ggplot(dt.crimes, aes(x = Primary.Type)) + geom_bar() + ggtitle("Occurences per crime type") + coord_flip() + labs(x = 'Crime')
  })
  
  #output$hist.Description <- renderPlot({
   # ggplot(dt.crimes, aes(x = Description)) + geom_bar() + ggtitle("Occurences per crime type - Description") + coord_flip()
  #})
  
 # output$tb.locations <- renderTable({
  #  dt.locations <- data.frame(Location = c("Wards", "Districts", "Beats", "Community Areas"), Total = c(length(unique(dt.crimes$Ward)), length(unique(dt.crimes$District)), length(unique(dt.crimes$Beat)), length(unique(dt.crimes$Community.Area))))
  #})
  
  output$tb.locations <- renderTable({
    dt.location <- data.frame(Location = c("Districts", "Wards", "Community Areas", "Beats", "Blocks"), 
                              Total = c(length(unique(dt.crimes$District)), length(unique(dt.crimes$Ward)), length(unique(dt.crimes$Community.Area)), length(unique(dt.crimes$Beat)), length(unique(dt.crimes$Block))), 
                              Mean = c("11518.48", "5298.5", "3440.584", "966.8796", "966.8796"), 
                              SD = c("4084.802", "3080.482", "2985.395", "391.9794", "216.28922"), 
                              Min = c("3", "2224", "268", "61", "1"), 
                              Max = c("17842", "17145", "15273", "3108", "946"))
  })
#}
  output$variables <- DT:: renderDT({
    dt.variables
  })
  
  output$sample <- DT:: renderDT({
    
    DT::datatable(
      dt.crimes %>% 
        dplyr::sample_n(100),
      filter = 'top', extensions = c('Buttons', 'FixedColumns'),
      options = list(
        scrollX = 500,
        deferRender = TRUE,
        scroller = TRUE,
        paging = TRUE,
        pageLength = 10,
        buttons = list('excel',
                       list(extend = 'colvis', targets = 0, visible = FALSE)),
        dom = 'lBfrtip',
        fixedColumns = list(leftColumns = 1)), 
      rownames = FALSE)
  })
  
  output$df.top.10.crimes <- renderTable({
    df.top.10.crimes <- data.frame(Crime = c((head(dt.freq.crimes$Primary.Type, 10))), Daily = c(trunc(head(dt.freq.crimes$N, 10)/360)), Hourly = c(trunc(head(dt.freq.crimes$N, 10)/(360*24))))
  })
  
  
  # -- Crime Tab
  tabledata.ward <- function(){
    dt.crimes[Primary.Type %in% input$crime.type][Date.Time >= input$daterange.crime[1] & Date.Time <= input$daterange.crime[2]][, Per_Ward := .N[1L], by = list('Crime' = Primary.Type, Ward)][, Total := .N[1L], by = Crime][, list(Ward, Per_Ward, Total)][!duplicated(Ward), ][order(-Per_Ward)]
  }
  histdata.ward <- function(){
    dt.crimes.hist.wards <- dt.crimes[Primary.Type %in% input$crime.type][Date.Time >= input$daterange.crime[1] & Date.Time <= input$daterange.crime[2]]
    dt.crimes.hist.wards %>% ggplot(aes(x = Ward)) + geom_bar() + coord_flip()
  }
  output$summarytable.crime <- DT::renderDT({
    tabledata.ward()
  })
  output$hist.crime <- renderPlot({
    histdata.ward()
  })
  
  # -- Ward Tab
  
  dt.ward.table <- as.data.table(dt.ward)
 
   demodata <-function(){
    dt.ward.table[Ward %in% input$ward][, "Pct.White" := Race.White_pct * 100][, "Pct.Asian" := Race.Asian_pct * 100][, "Pct.Black" := Race.Black_pct * 100][, "Pct.Hispanic" := Ethnicity.Hispanic_pct * 100][, "IncomeUSD"  := Income][, list(Pct.White, Pct.Asian, Pct.Black, Pct.Hispanic, IncomeUSD)]
    
  }
  
  tabledata <- function(){
    dt.crimes[Ward %in% input$ward][Date.Time >= input$daterange[1] & Date.Time <= input$daterange[2]][, 'Per_Crime' := .N[1L], by = list(Primary.Type)][, 'Total' := .N[1L], by = Ward][, list(Primary.Type, Per_Crime, Total)][!duplicated(Primary.Type), ][order(-Per_Crime)]
  }
  histdata <- function(){
    dt.crimes.hist <- dt.crimes[Ward %in% input$ward][Date.Time >= input$daterange[1] & Date.Time <= input$daterange[2]]
    dt.crimes.hist %>% ggplot(aes(x = Primary.Type)) + geom_bar() + coord_flip() + labs(x = 'Crime')
  }
  
  output$demotable <- DT::renderDT({
    demodata()
  })
  
  output$summarytable <- DT::renderDT({
    tabledata()
  })
  
  output$hist <- renderPlot({
    histdata()
  })
  
  # ---- Interactive Map ----
  
  # - Use the sampled crimes to build Choropleth spatial data frame
  # Count number of crimes per ward
  dt.crimes.map <- as.data.table(dt.crimes.map)
  dt.crimes.map.choropleth <- dt.crimes.map[, n_crimes := .N, by = Ward]
  # Delete duplicate wards and keep only ward and n_crimes variables
  dt.crimes.map.choropleth <- dt.crimes.map.choropleth[!duplicated(dt.crimes.map.choropleth$Ward), 
                                                       list(Ward, n_crimes)]
  # Merge the spatial data frame with crime count by ward data
  sp.ward.boundaries.map <- sp::merge(sp.ward.boundaries, dt.crimes.map.choropleth, 
                                      by.x = "ward", by.y = "Ward", all.x = TRUE)
  
  # - Calculate color bins for legend
  # Use quantiles function to find dynamic bin cutoffs
  bins.quantiles <- quantile(sp.ward.boundaries.map$n_crimes, names = FALSE, 
                             probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
  # Store bins
  bins <- c(0, bins.quantiles, Inf)
  # Assign different color to each bin
  pal2 <- colorBin("YlOrRd", domain = sp.ward.boundaries.map$n_crimes, bins = bins)
  
  output$text1 <- renderText({paste("Hello Wesley")})
  
  dt.crimes.map.filter <- reactive({
    dt.crimes.map[Type.Highlight %in% input$Primary.Type &
                    #Location.Description %in% input$Location.Description &
                    Arrest %in% input$Arrest &
                    Domestic %in% input$Domestic &
                    Ward >= input$Ward[1] & Ward <= input$Ward[2] &
                    date(Date.Time) >= input$Date[1] & date(Date.Time) <= input$Date[2], ]
  })
  
  sp.ward.boundaries.map.filter <- reactive({
    dt.crimes.map.filter2 <- dt.crimes.map[Type.Highlight %in% input$Primary.Type &
                                            #Location.Description %in% input$Location.Description &
                                             Arrest %in% input$Arrest &
                                             Domestic %in% input$Domestic &
                                             Ward >= input$Ward[1] & Ward <= input$Ward[2] &
                                             date(Date.Time) >= input$Date[1] & date(Date.Time) <= input$Date[2], ]
    dt.crimes.map.choropleth.filter <- dt.crimes.map.filter2[, n_crimes := .N, by = list(Ward)]
    dt.crimes.map.choropleth.filter <- dt.crimes.map.choropleth.filter[!duplicated(dt.crimes.map.choropleth.filter$Ward), 
                                                                       list(Ward, n_crimes)]
    outcome2 <- sp::merge(sp.ward.boundaries, dt.crimes.map.choropleth.filter, 
                          by.x = "ward", by.y = "Ward", all.x = TRUE)
    outcome2
  })
  
  pal2.filter <- reactive({
    dt.crimes.map.filter3 <- dt.crimes.map[Type.Highlight %in% input$Primary.Type &
                                             #Location.Description %in% input$Location.Description &
                                             Arrest %in% input$Arrest &
                                             Domestic %in% input$Domestic &
                                             Ward >= input$Ward[1] & Ward <= input$Ward[2] &
                                             date(Date.Time) >= input$Date[1] & date(Date.Time) <= input$Date[2], ]
    dt.crimes.map.choropleth.filter3 <- dt.crimes.map.filter3[, n_crimes := .N, by = list(Ward)]
    dt.crimes.map.choropleth.filter3 <- dt.crimes.map.choropleth.filter3[!duplicated(dt.crimes.map.choropleth.filter3$Ward), 
                                                                         list(Ward, n_crimes)]
    sp.ward.boundaries.map.filter3 <- sp::merge(sp.ward.boundaries, dt.crimes.map.choropleth.filter3, 
                                                by.x = "ward", by.y = "Ward", all.x = TRUE)
    bins.quantiles3 <- unique(quantile(sp.ward.boundaries.map.filter3$n_crimes, names = FALSE, 
                                       probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = TRUE))
    bins3 <- c(0, bins.quantiles3, Inf)
    outcome3 <- colorBin("YlOrRd", domain = sp.ward.boundaries.map.filter3$n_crimes, bins = bins3)
    outcome3
  })
  
  output$m.crimes <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>% 
      addPolygons(data = sp.ward.boundaries.map, fillOpacity = 0, color = "#000000", 
                  weight = 1, opacity = 1, smoothFactor = 0, 
                  label = paste0("Ward: ", sp.ward.boundaries.map$ward),
                  labelOptions = labelOptions(interactive = TRUE, textsize = "15px")) %>% 
      addCircleMarkers(data = dt.crimes.map, 
                       lng = ~Longitude, 
                       lat = ~Latitude, 
                       popup = ~paste0("<h4>", Primary.Type,"</h4>",
                                       "<b>Date/Time: </b>", Date.Time,
                                       "<br><b>Ward: </b>", Ward,
                                       "<br><b>Primary Type: </b>", Primary.Type,
                                       "<br><b>Specific Type: </b>", Description,
                                       "<br><b>Location: </b>", Location.Description,
                                       "<br><b>Someone Arrested: </b>", Arrest,
                                       "<br><b>Domestic Incident: </b>", Domestic,
                                       "<br><b>Block: </b>", Block),
                       radius = ~ifelse(Arrest == "Yes", 5, 5),
                       color = ~pal(Type.Highlight),
                       stroke = FALSE, fillOpacity = 0.4) %>% 
      addLegend("topright", pal = pal, values = l.Type.Highlight,
                title = "Primary Type",
                opacity = 1)
  })
  
  observe({
    leafletProxy("m.crimes", data = dt.crimes.map.filter()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude, 
        popup = ~paste0("<h4>", Primary.Type, "</h4>",
                        "<b>Date/Time: </b>", Date.Time,
                        "<br><b>Ward: </b>", Ward,
                        "<br><b>Primary Type: </b>", Primary.Type,
                        "<br><b>Specific Type: </b>", Description,
                        "<br><b>Location: </b>", Location.Description,
                        "<br><b>Someone Arrested: </b>", Arrest,
                        "<br><b>Domestic Incident: </b>", Domestic,
                        "<br><b>Block: </b>", Block),
        radius = ~ifelse(Arrest == "Yes", 5, 5),
        color = ~pal(Type.Highlight),
        stroke = FALSE, fillOpacity = 0.4)
  })
  
  output$m.crimes.choropleth <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>% 
      addPolygons(data = sp.ward.boundaries.map, 
                  fillColor = ~pal2(sp.ward.boundaries.map$n_crimes),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  smoothFactor = 0, 
                  label = ~paste0("Ward ", sp.ward.boundaries.map$ward, " -- ",
                                  "# of Crimes: ", sp.ward.boundaries.map$n_crimes),
                  labelOptions = labelOptions(interactive = TRUE, textsize = "15px"),
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>% 
      addLegend(pal = pal2, values = sp.ward.boundaries.map$n_crimes, opacity = 1, title = "# of Crimes",
                position = "topright")
  })
  
  observe({
    sp.ward.boundaries.map.filter.save <- sp.ward.boundaries.map.filter()
    pal2.filter.save <- pal2.filter()
    leafletProxy("m.crimes.choropleth", data = sp.ward.boundaries.map.filter.save) %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(
        fillColor = ~pal2.filter.save(sp.ward.boundaries.map.filter.save$n_crimes),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        smoothFactor = 0, 
        label = ~paste0("Ward ", sp.ward.boundaries.map.filter.save$ward, " -- ",
                        "# of Crimes: ", sp.ward.boundaries.map.filter.save$n_crimes),
        labelOptions = labelOptions(interactive = TRUE, textsize = "15px"),
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)) %>% 
      addLegend(pal = pal2.filter.save, values = sp.ward.boundaries.map.filter.save$n_crimes, opacity = 1, title = "# of Crimes",
                position = "topright")
  })
  
  # ---- Network Exploration ----
 
  crimes <- reactive({
    dt.crimes.filtered <- dt.crimes.filtered[Primary.Type %in% input$crimetypes, ]
  })
  
  
  
  mergekey <- reactive({
    
    if (input$edge == 'Crime Type') {
      mergekey <- 'Primary.Type'
    } else if (input$location == 'Ward' & input$edge == 'Location') {
      mergekey <- 'Ward'
    } else if (input$location == 'District' & input$edge == 'Location') {
      mergekey <- 'District'
    }
  })
  
  
  edgelist <- reactive({
    
    if (input$location == 'Ward' & input$edge == 'Crime Type') {
      edgelist <- merge(dt.crimes.filtered, dt.crimes.filtered, by = mergekey(), allow.cartesian = TRUE)
      edgelist <- edgelist[!duplicated(edgelist)][Date.Time.x >= input$daterange[1] & Date.Time.y >= input$daterange[1] & Date.Time.x <= input$daterange[2] & Date.Time.y <= input$daterange[2], ]
      edgelist <- edgelist[, list(Primary.Type, Ward.x, Ward.y)][Ward.x != Ward.y, ]
      edgelist <- edgelist[, weight := .N, by = list(Ward.x, Ward.y)][!duplicated(edgelist)]
      edgelist$min <- with(edgelist, pmin(Ward.x, Ward.y))
      edgelist$max <- with(edgelist, pmax(Ward.x, Ward.y))
      edgelist <- edgelist %>% mutate(Primary.Type.Edge = paste(Primary.Type, min, max))
      edgelist <- edgelist[!duplicated(Primary.Type.Edge)][, list(Ward.x, Ward.y, weight, Primary.Type)]
      edgelist <- as.matrix(edgelist[, list(Ward.x, Ward.y, weight)])
    } else if (input$location == 'District' & input$edge == 'Crime Type') {
      edgelist <- merge(dt.crimes.filtered, dt.crimes.filtered, by = mergekey(), allow.cartesian = TRUE)
      edgelist <- edgelist[!duplicated(edgelist)][Date.Time.x >= input$daterange[1] & Date.Time.y >= input$daterange[1] & Date.Time.x <= input$daterange[2] & Date.Time.y <= input$daterange[2], ]
      edgelist <- edgelist[, list(Primary.Type, District.x, District.y)][District.x != District.y, ][, weight := .N, by = list(District.x, District.y)]
      edgelist <- edgelist[!duplicated(edgelist)]
      edgelist$min <- with(edgelist, pmin(District.x, District.y))
      edgelist$max <- with(edgelist, pmax(District.x, District.y))
      edgelist <- edgelist %>% mutate(Primary.Type.Edge = paste(Primary.Type, min, max))
      edgelist <- edgelist[!duplicated(Primary.Type.Edge), list(District.x, District.y, weight, Primary.Type)]
      edgelist <- as.matrix(edgelist[, list(District.x, District.y, weight)])
    } else if (input$location == 'Ward' & input$edge == 'Location') {
      edgelist <- merge(dt.crimes.filtered, dt.crimes.filtered, by = mergekey(), allow.cartesian = TRUE)
      edgelist <- edgelist[!duplicated(edgelist)][Date.Time.x >= input$daterange[1] & Date.Time.y >= input$daterange[1] & Date.Time.x <= input$daterange[2] & Date.Time.y <= input$daterange[2], ]
      edgelist <- edgelist[, list(Ward, Primary.Type.x, Primary.Type.y)][Primary.Type.x != Primary.Type.y, ][, weight := .N, by = list(Primary.Type.x, Primary.Type.y, Ward)]
      edgelist <- edgelist[!duplicated(edgelist)]
      edgelist <- edgelist[!duplicated(lapply(as.data.frame(t(edgelist), stringsAsFactors=FALSE), sort)),][, list(Primary.Type.x, Primary.Type.y, weight, Ward)]
      edgelist <- as.matrix(edgelist[, list(Primary.Type.x, Primary.Type.y, weight)])
    } else if (input$location == 'District' & input$edge == 'Location') {
      edgelist <- merge(dt.crimes.filtered, dt.crimes.filtered, by = mergekey(), allow.cartesian = TRUE)
      edgelist <- edgelist[!duplicated(edgelist)][Date.Time.x >= input$daterange[1] & Date.Time.y >= input$daterange[1] & Date.Time.x <= input$daterange[2] & Date.Time.y <= input$daterange[2], ]
      edgelist <- edgelist[, list(District, Primary.Type.x, Primary.Type.y)][Primary.Type.x != Primary.Type.y, ][, weight := .N, by = list(Primary.Type.x, Primary.Type.y, District)]
      edgelist <- edgelist[!duplicated(edgelist)]
      edgelist <- edgelist[!duplicated(lapply(as.data.frame(t(edgelist), stringsAsFactors=FALSE), sort)),][, list(Primary.Type.x, Primary.Type.y, weight, District)]
      edgelist <- as.matrix(edgelist[, list(Primary.Type.x, Primary.Type.y, weight)])
    }
  })
  
  graphObject <- reactive({
    
    g.graph <- graph.edgelist(edgelist()[, 1:2], directed = FALSE)
    E(g.graph)$weight <- as.numeric(edgelist()[, 3])
    V(g.graph)$strength <- igraph::strength(g.graph)
    V(g.graph)$closeness <- igraph::closeness(g.graph)
    V(g.graph)$betweenness <- igraph::betweenness(g.graph)
    V(g.graph)$evcent <- igraph::eigen_centrality(g.graph)$vector
    g.graph
  })
  
  plotObject <- reactive({
    g.plot <- graphObject()
    kept <- c(which(E(g.plot)$weight >= input$weightrange[1], which(E(g.plot)$weight <= input$weightrange[2])))
    g.plot <- subgraph.edges(g.plot, kept) 
    E(g.plot)$weight <- as.numeric(edgelist()[, 3])
    V(g.plot)$strength <- igraph::strength(g.plot)
    V(g.plot)$closeness <- igraph::closeness(g.plot)
    V(g.plot)$betweenness <- igraph::betweenness(g.plot)
    V(g.plot)$evcent <- igraph::eigen_centrality(g.plot)$vector
    g.plot
  })
  
  visObject <- reactive({
    g.ob <- plotObject()
    ## convert to VisNetwork-list
    visn.graph <- visNetwork::toVisNetworkData(g.ob)
    ## copy column "weight" to new column "value" in list "edges"
    visn.graph$edges$value <- g.ob$edges$weight
    
    visNetwork(nodes = visn.graph$nodes, edges = visn.graph$edges, height = "500px") %>% visNetwork::visIgraphLayout(layout = 'layout_nicely')
  })
  
  topcentralities <- reactive({
    dt.g.graph <- data.table::data.table(igraph::get.data.frame(plotObject(), "vertices"))
    if (input$edge == 'Crime Type') {
      dt.g.graph <- dt.g.graph[, name := rownames(dt.g.graph)] }
    dt.g.graph.order <- data.frame(strengthname = head(dt.g.graph[order(-strength)], 10)$name,
                                   strength = head(dt.g.graph[order(-strength)], 10)$strength,
                                   closenessname = head(dt.g.graph[order(-closeness)], 10)$name,
                                   closeness = head(dt.g.graph[order(-closeness)], 10)$closeness,
                                   betweennessname = head(dt.g.graph[order(-betweenness)], 10)$name,
                                   betweenness = head(dt.g.graph[order(-betweenness)], 10)$ betweenness,
                                   evcentname = head(dt.g.graph[order(-evcent)], 10)$name,
                                   evcent = head(dt.g.graph[order(-evcent)], 10)$evcent) 
    dt.g.graph.order
  })
  
  centralitystats <- reactive({ 
    
    dt.g.graph <- data.table::data.table(igraph::get.data.frame(plotObject(), 'vertices'))
    
    if (input$centrality == "Betweenness Centrality") {
      dt.g.graph <- data.frame(Minimum = min(dt.g.graph$betweenness),
                               Maximum = max(dt.g.graph$betweenness),
                               StandardDev = sd(dt.g.graph$betweenness),
                               Mean = mean(dt.g.graph$betweenness)
                               )
      
    } else if (input$centrality == "Weighted Degree Centrality") {
      dt.g.graph <- data.frame(Minimum = min(dt.g.graph$strength),
                               Maximum = max(dt.g.graph$strength),
                               StandardDev = sd(dt.g.graph$strength),
                               Mean = mean(dt.g.graph$strength)
                               )
      
    } else if (input$centrality == "Closeness Centrality") {
      dt.g.graph <- data.frame(Minimum = min(dt.g.graph$closeness),
                               Maximum = max(dt.g.graph$closeness),
                               StandardDev = sd(dt.g.graph$closeness),
                               Mean = mean(dt.g.graph$closeness)
                               )
      
    } else if (input$centrality == "Weighted Eigenvector Centrality") {
      dt.g.graph <- data.frame(Minimum = min(dt.g.graph$evcent),
                               Maximum = max(dt.g.graph$evcent),
                               StandardDev = sd(dt.g.graph$evcent),
                               Mean = mean(dt.g.graph$evcent)
                               )
    }
    dt.g.graph
  })
  
  
  network.descriptives <- reactive ({
    g.graph <- plotObject()
    dt.descriptives.network <- data.frame(Measure = c("N. nodes", "N. edges", "Diameter", "Av. Path Length", "Clustering Coefficient"), Statistic = c(length(V(g.graph)), length(E(g.graph)), diameter(g.graph), average.path.length(g.graph), transitivity(g.graph)))
    
    dt.descriptives.network
    
  })
  
  
  output$centralitystats <- renderTable({
    centralitystats()
  })  
  
  
  output$network.descriptives <- renderTable ({
    network.descriptives()
  })
  
  
  output$graph <- visNetwork::renderVisNetwork({
    visObject()
    
  })
  
  output$strengthdist <- renderPlot({
    g.plot <- plotObject()
   # ggplot2::ggplot(igraph::strength(g.plot), aes(x = igraph::strength(g.plot))) + geom_histogram() + bins 
    ggplot2::qplot(igraph::strength(g.plot), geom = 'histogram', binwidth = input$binwidth) + labs(x = 'Weighted Degree')
  })
  
  output$topcentralities <- renderTable({
    topcentralities()
  })  
  
  output$moreControls <- renderUI({
    g.g <- graphObject()
    tagList(
      sliderInput('weightrange', 'Select Weight Range', min = min(E(g.g)$weight), max = max(E(g.g)$weight), value = c(((min(E(g.g)$weight) + max(E(g.g)$weight)) * 0.2), max(E(g.g)$weight))))
  })

  # ---- Advanced Analytics ----
  
  output$union <- visNetwork::renderVisNetwork({
    visNetwork::visNetwork(nodes = visn.union$nodes, edges = visn.union$edges, height = '1500px', width = '1000px') %>% visNetwork::visIgraphLayout(layout = 'layout_nicely')
  })
  
  output$regression.data <- DT::renderDT({
    dt.regression.data <- round(dt.ward, 2)
    DT::datatable(
      dt.regression.data,
      filter = 'top', extensions = c('Buttons', 'FixedColumns'),
      options = list(#scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     paging = TRUE,
                     pageLength = 10,
                     buttons = list('excel',
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = list(leftColumns = 1)), 
      rownames = FALSE)
  })
  
  output$regression.coefficients <- DT::renderDT({
    dt.regression.coefficients <- dt.regression.est
    dt.regression.coefficients <- dt.regression.coefficients %>% mutate_if(is.numeric, round, digits = 2)
    DT::datatable(
      dt.regression.coefficients,
      filter = 'top', extensions = c('Buttons', 'FixedColumns'),
      options = list(#scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     paging = TRUE,
                     pageLength = 10,
                     buttons = list('excel',
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = list(leftColumns = 1)), 
      rownames = FALSE)
  })
  
  predictions <- reactive({
    dt.output.prediction <- data.table(matrix(ncol = 2, nrow = 1))
    colnames(dt.output.prediction) <- c("Primary.Type", "Prediction")
    dt.all.predictions <- data.table(matrix(ncol = 2, nrow = 0))
    colnames(dt.all.predictions) <- c("Primary.Type", "Prediction")
    
    dt.manual.predictions <- c(input$Race.White_pct, 
                               input$Race.Black_pct, 
                               input$Race.Asian_pct,
                               input$Ethnicity.Hispanic_pct,
                               input$Income,
                               input$Abandoned_Vehicles,
                               input$Garbage_Carts,
                               input$Rodent_Baiting,
                               input$Sanitation_Code_Complaints,
                               input$Graffiti_Removal,
                               input$Abandoned_Buildings,
                               input$Police_Stations,
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Race.White_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Race.Black_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Race.Asian_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Ethnicity.Hispanic_pct"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Income"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Abandoned_Vehicles"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Garbage_Carts"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Rodent_Baiting"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Sanitation_Code_Complaints"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Graffiti_Removal"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Abandoned_Buildings"],
                               dt.ward[dt.ward$Ward == input$ward.input, "neig_Police_Stations"]
    )
    
    names(dt.manual.predictions) <- c(l.variables)
    
    for (ptype in l.primary.types) {
      predictor.values <- data.table(matrix(ncol = 0, nrow = 1))
      for (variables in l.variables) {
        value <- unname(dt.manual.predictions[variables])
        predictor.values <- cbind(predictor.values, value)
      }
      
      colnames(predictor.values) <- c(l.variables)
      
      prediction <- as.matrix(dt.regression.est[Primary.Type == ptype, ][, -c(1:2)]) * as.matrix(predictor.values)
      prediction <- sum(prediction) + dt.regression.est[Primary.Type == ptype, ]$Intercept
      
      if(round(prediction, 0) < 0) {
        dt.output.prediction$Primary.Type[1] <- ptype
        dt.output.prediction$Prediction[1] <- 0
        dt.all.predictions <- rbind(dt.all.predictions, dt.output.prediction)
      } else {
        dt.output.prediction$Primary.Type[1] <- ptype
        dt.output.prediction$Prediction[1] <- round(prediction, 0)
        dt.all.predictions <- rbind(dt.all.predictions, dt.output.prediction)
      }
    }
    dt.all.predictions
  })
  
  output$predictions <- DT::renderDT({
    predictions()
  })  
  
}

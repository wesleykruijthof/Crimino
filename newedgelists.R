##### Create edgelists as inputs for graphical objects
remove(list=ls()) #empty environment
set.seed(3) #Ensure reproducibility of the analysis
cat("\f")

setwd("~/MSc BIM/Current Courses/NDA/Group assignment/data") #Wesley
dt.crimes <- as.data.table(read.csv("Crimes.2017.csv"))

### Prepare data to be used
dt.crimes.filtered.location <- dt.crimes[, list(Primary.Type, Ward, District, Date.Time)]
dt.crimes.filtered.location$Date.Time <- as.Date(dt.crimes.filtered.location$Date.Time)

## Add column for projection with Locations as nodes
dt.crimes.filtered.location <- dt.crimes.filtered.location %>% mutate(Type.Time = paste(Primary.Type, Date.Time))
dt.crimes.filtered.location <- dt.crimes.filtered.location[duplicated(Type.Time), ]

# Create edgelist for Wards as nodes
edgelist <- merge(dt.crimes.filtered.location, dt.crimes.filtered.location, by = 'Type.Time', allow.cartesian = TRUE)
edgelist <- edgelist[!duplicated(edgelist)]
edgelist.wards <- edgelist[, list(Type.Time, Ward.x, Ward.y)][Ward.x != Ward.y, ]
edgelist.wards <- edgelist.wards[, weight := .N, by = list(Ward.x, Ward.y)][!duplicated(edgelist.wards)]
edgelist.wards$min <- with(edgelist.wards, pmin(Ward.x, Ward.y))
edgelist.wards$max <- with(edgelist.wards, pmax(Ward.x, Ward.y))
edgelist.wards <- edgelist.wards %>% mutate(Type.Time.Edge = paste(Type.Time, min, max))
edgelist.wards <- edgelist.wards[!duplicated(Type.Time.Edge)][, list(Ward.x, Ward.y, weight, Type.Time)]
edgelist.wards <- as.matrix(edgelist.wards[, list(Ward.x, Ward.y, weight)])
# Create plot for Wards as nodes
g.wards <- graph.edgelist(edgelist.wards[, 1:2], directed = FALSE)
E(g.wards)$weight <- as.numeric(edgelist.wards[, 3])
summary(g.wards)
# Undirected weighted network, 50 nodes (ward), 1676647 connections (type.time)
V(g.wards)$degree <- igraph::degree(g.wards)
V(g.wards)$closeness <- igraph::closeness(g.wards)
V(g.wards)$betweenness <- igraph::betweenness(g.wards)
V(g.wards)$evcent <- igraph::evcent(g.wards)$vector
dt.g.wards <- data.table::data.table(igraph::get.data.frame(g.wards, "vertices"))
dt.g.wards <- dt.g.wards[, name := rownames(dt.g.wards)]
dt.g.wards.order <- data.frame(degreename = head(dt.g.wards[order(-degree)], 20)$name,
                               degree = head(dt.g.wards[order(-degree)], 20)$degree,
                               closenessname = head(dt.g.wards[order(-closeness)], 20)$name,
                               closeness = head(dt.g.wards[order(-closeness)], 20)$closeness,
                               betweennessname = head(dt.g.wards[order(-betweenness)], 20)$name,
                               betweenness = head(dt.g.wards[order(-betweenness)], 20)$ betweenness,
                               evcentname = head(dt.g.wards[order(-evcent)], 20)$name,
                               evcent = head(dt.g.wards[order(-evcent)], 20)$evcent)

# Create edgelist for Districts as nodes
edgelist.districts <- edgelist[, list(Type.Time, District.x, District.y)][District.x != District.y, ][, weight := .N, by = list(District.x, District.y)]
edgelist.districts <- edgelist.districts[!duplicated(edgelist.districts)]
edgelist.districts$min <- with(edgelist.districts, pmin(District.x, District.y))
edgelist.districts$max <- with(edgelist.districts, pmax(District.x, District.y))
edgelist.districts <- edgelist.districts %>% mutate(Type.Time.Edge = paste(Type.Time, min, max))
edgelist.districts <- edgelist.districts[!duplicated(Type.Time.Edge), list(District.x, District.y, weight, Type.Time)]
edgelist.districts <- as.matrix(edgelist.districts[, list(District.x, District.y, weight)])
# Create a plot for Districts as nodes
g.districts <- graph.edgelist(edgelist.districts[, 1:2], directed = FALSE)
E(g.districts)$weight <- as.numeric(edgelist.districts[, 3])
summary(g.districts)
# Undirected weighted network with 31 nodes (District) and 607474 edges (Type.Time)
V(g.districts)$degree <- igraph::degree(g.districts)
V(g.districts)$closeness <- igraph::closeness(g.districts)
V(g.districts)$betweenness <- igraph::betweenness(g.districts)
V(g.districts)$evcent <- igraph::evcent(g.districts)$vector
dt.g.districts <- data.table::data.table(igraph::get.data.frame(g.districts, "vertices"))
dt.g.districts <- dt.g.districts[, name := rownames(dt.g.districts)]
dt.g.districts.order <- data.frame(degreename = head(dt.g.districts[order(-degree)], 20)$name,
                                   degree = head(dt.g.districts[order(-degree)], 20)$degree,
                                   closenessname = head(dt.g.districts[order(-closeness)], 20)$name,
                                   closeness = head(dt.g.districts[order(-closeness)], 20)$closeness,
                                   betweennessname = head(dt.g.districts[order(-betweenness)], 20)$name,
                                   betweenness = head(dt.g.districts[order(-betweenness)], 20)$ betweenness,
                                   evcentname = head(dt.g.districts[order(-evcent)], 20)$name,
                                   evcent = head(dt.g.districts[order(-evcent)], 20)$evcent)

## Add column for projection with Primary.Types as nodes and Wards as edges
dt.crimes.filtered.crimes <- dt.crimes.filtered.location %>% mutate(Ward.Time = paste(Ward, Date.Time))
dt.crimes.filtered.crimes <- dt.crimes.filtered.crimes[duplicated(Ward.Time), ]
# Create edgelist for Primary Types as nodes and Wards as edges
edgelist.primary.wards <- merge(dt.crimes.filtered.crimes, dt.crimes.filtered.crimes, by = 'Ward.Time', allow.cartesian = TRUE)
edgelist.primary.wards <- edgelist.primary.wards[!duplicated(edgelist.primary.wards)]
edgelist.primary.wards <- edgelist.primary.wards[, list(Ward.Time, Primary.Type.x, Primary.Type.y)][Primary.Type.x != Primary.Type.y, ][, weight := .N, by = list(Primary.Type.x, Primary.Type.y, Ward.Time)]
edgelist.primary.wards <- edgelist.primary.wards[!duplicated(edgelist.primary.wards)]
# --- This works, but now the weights don't add up when remove the inversed duplicated THEY DONT HAVE TO!
edgelist.primary.wards <- edgelist.primary.wards[!duplicated(lapply(as.data.frame(t(edgelist.primary.wards), stringsAsFactors=FALSE), sort)),][, list(Primary.Type.x, Primary.Type.y, weight, Ward.Time)]
# adjacency matrix for primary wards
edgelist.primary.wards <- as.matrix(edgelist.primary.wards[, list(Primary.Type.x, Primary.Type.y, weight)])
# create plot for primary wards
g.primary.wards <- graph.edgelist(edgelist.primary.wards[, 1:2], directed = FALSE)
E(g.primary.wards)$weight <- as.numeric(edgelist.primary.wards[, 3])
summary(g.primary.wards)
# Undirected weighted network with 30 nodes (Primary.Type) and 353796 edges (Ward) 
V(g.primary.wards)$degree <- igraph::degree(g.primary.wards)
V(g.primary.wards)$closeness <- igraph::closeness(g.primary.wards)
V(g.primary.wards)$betweenness <- igraph::betweenness(g.primary.wards)
V(g.primary.wards)$evcent <- igraph::evcent(g.primary.wards)$vector
dt.g.primary.wards <- data.table::data.table(igraph::get.data.frame(g.primary.wards, "vertices"))
dt.g.primary.wards <- dt.g.primary.wards[, name := rownames(dt.g.primary.wards)]
dt.g.primary.wards.order <- data.frame(degreename = head(dt.g.primary.wards[order(-degree)], 20)$name,
                                       degree = head(dt.g.primary.wards[order(-degree)], 20)$degree,
                                       closenessname = head(dt.g.primary.wards[order(-closeness)], 20)$name,
                                       closeness = head(dt.g.primary.wards[order(-closeness)], 20)$closeness,
                                       betweennessname = head(dt.g.primary.wards[order(-betweenness)], 20)$name,
                                       betweenness = head(dt.g.primary.wards[order(-betweenness)], 20)$ betweenness,
                                       evcentname = head(dt.g.primary.wards[order(-evcent)], 20)$name,
                                       evcent = head(dt.g.primary.wards[order(-evcent)], 20)$evcent)

## Add column for projection with Primary.Types as nodes and Districts as edges
dt.crimes.filtered.crimes.districts <- dt.crimes.filtered.location %>% mutate(District.Time = paste(District, Date.Time))
dt.crimes.filtered.crimes.districts <- dt.crimes.filtered.crimes.districts[duplicated(District.Time), ]
# Create edgelist for Primary Types as nodes and districts as edges
edgelist.primary.districts <- merge(dt.crimes.filtered.crimes.districts, dt.crimes.filtered.crimes.districts, by = 'District.Time', allow.cartesian = TRUE)
edgelist.primary.districts <- edgelist.primary.districts[!duplicated(edgelist.primary.districts)]
edgelist.primary.districts <- edgelist.primary.districts[, list(District.Time, Primary.Type.x, Primary.Type.y)][Primary.Type.x != Primary.Type.y, ][, weight := .N, by = list(Primary.Type.x, Primary.Type.y, District.Time)]
edgelist.primary.districts <- edgelist.primary.districts[!duplicated(edgelist.primary.districts)]
# --- This works, but now the weights don't add up when remove the inversed duplicated THEY DONT HAVE TO!
edgelist.primary.districts <- edgelist.primary.districts[!duplicated(lapply(as.data.frame(t(edgelist.primary.districts), stringsAsFactors=FALSE), sort)),][, list(Primary.Type.x, Primary.Type.y, weight, District.Time)]
# adjacency matrix for primary districts
edgelist.primary.districts <- as.matrix(edgelist.primary.districts[, list(Primary.Type.x, Primary.Type.y, weight)])
# create plot for primary wards
g.primary.districts <- graph.edgelist(edgelist.primary.districts[, 1:2], directed = FALSE)
E(g.primary.districts)$weight <- as.numeric(edgelist.primary.districts[, 3])
summary(g.primary.districts)
# Undirected weighted network with 30 nodes (Primary.Type) and 346535 edges (District)
V(g.primary.districts)$degree <- igraph::degree(g.primary.districts)
V(g.primary.districts)$closeness <- igraph::closeness(g.primary.districts)
V(g.primary.districts)$betweenness <- igraph::betweenness(g.primary.districts)
V(g.primary.districts)$evcent <- igraph::evcent(g.primary.districts)$vector
dt.g.primary.districts <- data.table::data.table(igraph::get.data.frame(g.primary.districts, "vertices"))
dt.g.primary.districts <- dt.g.primary.districts[, name := rownames(dt.g.primary.districts)]
dt.g.primary.districts.order <- data.frame(degreename = head(dt.g.primary.districts[order(-degree)], 20)$name,
                                           degree = head(dt.g.primary.districts[order(-degree)], 20)$degree,
                                           closenessname = head(dt.g.primary.districts[order(-closeness)], 20)$name,
                                           closeness = head(dt.g.primary.districts[order(-closeness)], 20)$closeness,
                                           betweennessname = head(dt.g.primary.districts[order(-betweenness)], 20)$name,
                                           betweenness = head(dt.g.primary.districts[order(-betweenness)], 20)$ betweenness,
                                           evcentname = head(dt.g.primary.districts[order(-evcent)], 20)$name,
                                           evcent = head(dt.g.primary.districts[order(-evcent)], 20)$evcent)
vertex_attr(g.primary.districts) <- list(name = dt.g.primary.districts$name,
                       color = rep("yellow", gorder(g.primary.districts)))
vertex_attr(g.primary.districts, "label") <- V(g.primary.districts)$name
plot(g.primary.districts, vertex.size = 1)

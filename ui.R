navbarPage("Crimino", collapsible = TRUE, inverse = TRUE, theme = "bootstrap.css",
           tabPanel("About",
                    sidebarLayout(
                      sidebarPanel = sidebarPanel(h1("Welcome to Crimino!"),
                                                  h5("Connecting the dots of Chicago's crime patterns."),
                                                  img(src = "Chicago.bean.png", height = 400, width = 400, align = "center")),
                      mainPanel = mainPanel(
                        tabsetPanel(
                           tabPanel("Platform", 
                                     br(),
                            p("Crimino is a platform which aims to shed light on patterns of crime in the city of Chicago. 
By making use of a dataset extracted from the Chicago Police department for the year 2017, this platform offers you the possibility to view and analyze criminal data in different formats."),

p("The general aim of Crimino is to expose patterns that could not previously be seen by the naked eye and help law enforcement distinguish connections between crime type, location and time."),

p("In addition to the overview provided in Data explorer and Interactive map, Crimino provides an advanced analysis to try and predict crime cooccurrence based on various user inputs.")),
                                 tabPanel("Relevance",
                                          br(),
                                          p(" One of the drawbacks of having separate law enforcement entities dedicated to distinct locations is that sometimes, one can lose sight of the overall picture. 
                                            Officers working on different stations, or even on different crime types, do not get the chance to compare notes -- 
                                            as there is too much going on as you will see through Crimino."),
                                   p(" In comes Crimino, where crime analysts can combine data from different perpectives and infer different patterns from them.   
                                            In addition to law enforcement officials, Crimino also aims to give Chicago citizens the opportunity to have an overview of crime in their location."),
                                            p("Having this overview, gives authorities the opportunity to devise efficient measures of crime control and proactively fight it.")),
                                            
                        
                            tabPanel("Team",
                                     br(),
                                     p("Meet the team behind Crimino! A team of students of Business Information Management at RSM who aim to help you explore the crime network in Chicago."),
                                     br(),
img(src = "Klea.png", height = 100, width = 200),
p("<b>","Klea Gjana","</b>"),
p("Business Information student"),

img(src = "Wesley.png", height = 100, width = 200),
p("<b>","Wesley Kruijthof","</b>"),
p("Business Information student"),

img(src = "Maria.png", height = 100, width = 200),
p("<b>","Maria Pere de Melo","</b>"),
p("Business Information student"),

img(src = "Jelle.png", height = 100, width = 200),
p("<b>","Jelle van der Grijn","</b>"),
p("Business Information student")
                          
                        )))),
           ),
           tabPanel("Data Explorer",
                         h1("Data Explorer"),
                    br(),
                    p("Have you ever wondered what the most recurring crime type was in a specific location or specific point in time? Data Explorer allows you to check crime statistics for your ward in the desired date range."),
                    fluidPage(
                      tabsetPanel(
                        tabPanel("Explore the Dataset", 
                                 fluidPage (
                                   titlePanel("Data Explorer Descriptives"),
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                                p("Here you can find overall descriptives on the crime in Chicago in 2017. There are in total 254 different types of crimes happening accross 50 wards. 
                                                       In total, there are around 30.000 crime cases in more that 23.000 different specific locations"),
                                              br(),
                                       tableOutput("tb.descriptives")
                                     ),
                                     
                                     mainPanel (
                                       tabsetPanel(
                                         tabPanel("Wards", plotOutput("hist.ward")),
                                         tabPanel("Crimes", plotOutput("hist.Primary.Type")),
                                         tabPanel("Crime Descriptions", plotOutput("hist.Description"))
                                       ),
                                       tableOutput("tb.locations")
                                     )
                                   )
                                 )
                                 ),
                        tabPanel("Explore Crime", 
                                 titlePanel('Data Explorer Crime Types'),
                                 sidebarLayout(
                                   sidebarPanel(
                                              p(" Through the 'Data Explorer Crime Types' you can choose a crime type and check the frequency of this crime type per ward in the specified date range. 
                                                       You also have the possibility to check the total amount that this crime has happened in all wards in the specified date range under the column 'Total'."),
                                            br(), 
                                     selectInput(inputId = 'crime.type', label = 'Select Crime Trype', 
                                                 choices = c(as.vector(unique(dt.crimes$Primary.Type)), 'All Crimes')),
                                     dateRangeInput(inputId = 'daterange.crime', label = 'Select Date Range', 
                                                    start = '2017-01-01', end = '2017-12-31', format = 'yyyy-mm-dd')),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel('Overview', 
                                                {DT::DTOutput('summarytable.crime')}),
                                       tabPanel('Chart',
                                                {plotOutput('hist.crime')})
                                     )))),
                        tabPanel("Explore Wards", 
                                 titlePanel('Data Explorer Wards'),
                                 sidebarLayout(
                                   sidebarPanel(
                                              p(" Through the 'Data Explorer Wards' you can choose a ward and check the crime types that have happened in that ward in the specified date range.
                                                       You also have the possibility to check how many times each crime type has happened and the total amount of that type of crime for the chosen ward.
                                                In addition you can get an overview of demographics and income level on the ward you've chosen."),
                                            br(),
                                     selectInput(inputId = 'ward', label = 'Select Ward', choices = c(as.vector(unique(dt.crimes$Ward)), 'All Wards')),
                                     dateRangeInput(inputId = 'daterange', label = 'Select Date Range', start = '2017-01-01', end = '2017-12-31', format = 'yyyy-mm-dd')),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel('Overview', 
                                                {DT::DTOutput('summarytable')},
                                                br(),
                                       {DT:DTOutput('demotable')}),
                                       tabPanel('Chart',
                                                {plotOutput('hist')})
                                     ))))
                      ))),
           tabPanel("Interactive Map",
                    fluidPage(
                      h1("Interactive Map"),
                               br(),
                      p("In order to get a feel of what the data looks like, feel free to use this interactive map, where crimes are plotted on the map of chicago by a simplified version of primary crime type.
                        The crimes plotted on the map are a random sample of 20,000 crimes taken from the original dataset of 264,925 crimes."),
                               br(),
                        p("The top graph shows all individual crimes plotted on a raster indicating the 50 wards.
                        The bottom graph is a choropleth graph aggregating the number of crimes by ward, creating a heatmap."),
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("Date", label = h4("Period"), start = "2017-03-01", end = "2017-10-31", 
                                         min = "2017-01-01", max = "2017-12-31"),
                          sliderInput("Ward", label = h4("Ward"), min = 1, max = 50, value = c(1, 50)),
                          checkboxGroupInput("Arrest", label = h4("Someone was arrested"), choices = c("Yes", "No"), 
                                             selected = c("Yes", "No")),
                          checkboxGroupInput("Domestic", label = h4("Domestic incident"), choices = c("Yes", "No"), 
                                             selected = c("Yes", "No")),
                          checkboxGroupInput("Primary.Type", label = h4("Primary crime type"), choices = l.Type.Highlight, 
                                             selected = l.Type.Highlight),
                          #checkboxGroupInput("Location.Description", label = h4("Location"), choices = l.Location.Description, 
                          #                   selected = l.Location.Description, inline = TRUE),
                        ),
                        mainPanel(
                          h2("Map showing individual crimes"),
                          leafletOutput("m.crimes", height = 500),
                          br(),
                          h2("Heatmap of crimes by ward"),
                          leafletOutput("m.crimes.choropleth", height = 500),
                        )
                      )
                    )
           ),
           tabPanel("Network Exploration",
                    h1("Network Exploration"),
                    br(),
                    p("What does the network of crime look like? How are two wards connected? In here you can choose between two projections of the crime dataset. Based on your choice, 
you can project crime types connected by location, which connects different crimes if they have happened in the same location. Vice-versa, you can project locations as connected by crimes.
This projection, allows you to check locations that are connected by having common crimes happening in the same day."),
                    fluidPage(
                      tabsetPanel(
                        tabPanel("Network Visualization", 
                                 titlePanel('Network Exploration'),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = 'node', label = 'Select Location Varibale', choices = c('District', 'Ward'), selected = 'District'),
                                     selectInput(inputId = 'edge', label = 'Choose Edges', choices = c('Crime Type', 'Location'), selected = 'Crime Type'),
                                     #sliderInput(inputId = 'degree', label = 'Select Degree Range', min = 0, max = 6000, value = 10),
                                     #dateRangeInput(inputId = 'daterange', label = 'Choose a Date Range', start = '2017-01-01', end = '2017-12-31', startview = 'month', separator = ' to '),
                                     selectInput(inputId = 'centrality', label = 'Select Centrality Measure', choices = c('Degree', 'Closeness Centrality', 'Betweenness Centrality', 'Eigenvector Centrality'), selected = 'Degree')),
                                   mainPanel(   
                                     threejs::scatterplotThreeOutput('graph'),
                                     #      DT::DTOutput("networksummary"),
                                     tableOutput("centralitysummary")
                                   ))
                                 ),
                        tabPanel("Interpretation Guide",
                                 titlePanel("Interpreting Network Descriptives"),
                                 br(),
                                 h2("Summary Statistics"),
                                 p(" The number of nodes represents the total number of crimes OR locations that are part of the network."),
p("The number of edges translates into the connections between the crimes and locations OR location and crimes"),
p("The average degree states how connected, in terms of direct links, the crimes OR locations are to each other."),
p("The clustering coefficient shows the average degree to which neighboring crimes OR locations within the network are connected among themselves. It shows how interconnected the projected network is."),
                               br(),
                                 h2(" Centralities"),
                                 p(" The centrality measures in a network indicate which node is the most central in the network. 
Depending on your choice, that would mean which crime connects most locations, or which locations connect most crimes."),
                                 p(" Degree Centrality: Measures how directly connected to other nodes a specific node is. 
Depending on your chosen projection that could mean: a crime that is connected to many other crimes has many common locations to other crimes 
                    OR a location is highly connected to other location and has many crimes in common happening in them."),
p("Closeness Centrality: Measures the average shortest path length from a node to every other node. 
Thus, the more central in the network a crime OR location is, the closer it is to all other nodes of the same type."),
                    p("Betwennes Centrality: Measures how important a specific crime type OR location is in terms of connecting to other nodes of the same type."),
                                 
                      )))), 
           tabPanel("Advanced Analytics",
                    titlePanel("Predicting Crime Occurrence"),
                    br(),
                    p("Through our predictive analysis of the crime dataset you can receive information about whether a certain crime station is likely to happen in a specific ward given that two wards are connected."),
)
)

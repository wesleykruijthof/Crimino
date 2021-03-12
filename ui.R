navbarPage("Crimino", collapsible = TRUE, inverse = TRUE, theme = "bootstrap.css",
           tabPanel("About",
                    sidebarLayout(
                      sidebarPanel = sidebarPanel(h1("Welcome to Crimino!"),
                                                  h5("Connecting the dots of Chicago's crime patterns."),
                                                  HTML('<center><img src="Chicago.bean.png" height="200" width="200"></center>'),
                      ),
                      mainPanel = mainPanel(
                        tabsetPanel(
                           tabPanel("Platform", 
                                     br(),
                            p("Crimino is a platform which aims to shed light on patterns of crime in the city of Chicago. 
By making use of a dataset extracted from the Chicago Police department for the year 2020, this platform offers you the possibility to view and analyze criminal data in different formats."),

p("The general aim of Crimino is to expose patterns that could not previously be seen by the naked eye and help law enforcement distinguish connections between crime type, location and time."),

p("In addition to the overview provided in Data explorer and Interactive map, Crimino provides an advanced analysis via link prediction to try and predict crime cooccurrence.
Finally, Crimino provides a node similarity analysis to spot similarities across the wards of Chicago in terms of crime type and demographics.")),
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
                                     fluidRow(
                                       column(3, 
img(src = "Klea.png", height = 100, width = 100),
img(src = "Wesley.png", height = 100, width = 100),
img(src = "Maria.png", height = 100, width = 100),
img(src = "Jelle.png", height = 100, width = 100)),

column(4, 
p("Klea Gjana"),
p("BIM student"),
br(),
br(),
br(),
p("Wesley Kruijthof"),
p("BIM student"),
br(),
br(),
br(),
p("Maria Peres de Melo"),
p("BIM student"),
br(),
br(),
br(),
p("Jelle van der Grijn"),
p("BIM student"))
                          
                        ))))),
           ),
           
           tabPanel("Data Explorer",
                         h1("Data Explorer"),
                    br(),
                    p("Have you ever wondered what the most recurring crime type was in a specific location or specific point in time? Data Explorer allows you to check crime statistics for your ward in the desired date range."),
                    fluidPage(
                      tabsetPanel(
                      tabPanel(
                                 titlePanel("The Crimino Dataset"),
                                 sidebarPanel(
                                            h4("Find out more about the dataset behind Crimino"),
                                            
                                            p("The top table at the right describes the 23 variables that were collected for each observation in the Crimino dataset."),
          
                                            p("Additionally,the bottom table displays a sample of 100 random observations from the dataset."),
      
                                            p("All our data was obtained from the Chicago Data Portal, the City of Chicago's open data platform. The link to the portal's website can be found below."),
      
                                            a(href = "https://data.cityofchicago.org", "Chicago Data Portal")
                                            
                                 ),
                                 
                                 mainPanel(
                                            h3("Variables Description"),
                                            DT::DTOutput("variables"),
                                            br(),
                                            hr(),
  
                                            h3("Dataset Sample"),
                                            DT::DTOutput("sample")
                                 )
                                 ),  
                      tabPanel("Explore the Dataset", 
                                 fluidPage (
                                   titlePanel("Data Explorer Descriptives"),
                                   
                                   sidebarLayout(
                                     
                                     sidebarPanel(
                                                h4("Explore the Crimino Dataset"),
                                                p("Here you can find overall descriptives on the crime in Chicago in 2017. There are in total 254 different types of crimes happening accross 50 wards. 
                                                       In total, there are around 30.000 crime cases in more that 28.000 different specific locations"),
                                              br(),
                                       tableOutput("tb.descriptives")
                                     ),
                                     
                                     mainPanel (
                                       tabsetPanel(
                                         tabPanel("Wards", h3("Frequency of Crimes per Ward"),plotOutput("hist.ward")),
                                         tabPanel("Crime Types",h3("Frequency of Crime Types"), plotOutput("hist.Primary.Type")),
                                         tabPanel("Top 10 Crimes", h3("Top 10 Crimes - Daily and Hourly Occurrence"), tableOutput("df.top.10.crimes"))
                                       ),
                                      
                                        hr(),
                                         h3("Data descriptives per type of Location"),
                                         tableOutput("tb.locations"),
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
                                                       You also have the possibility to check how many times each crime type has happened and the total amount of that type of crime for the chosen ward."),
                                            br(),
                                     selectInput(inputId = 'ward', label = 'Select Ward', choices = c(as.vector(unique(dt.crimes$Ward)), 'All Wards')),
                                     dateRangeInput(inputId = 'daterange', label = 'Select Date Range', start = '2017-01-01', end = '2017-12-31', format = 'yyyy-mm-dd')),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel('Overview', 
                                                {DT::DTOutput('summarytable')}),
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
                                       h2("Viewing Bipartite Network"),
                                       selectInput(inputId = 'location', label = 'Select Location Variable', choices = c('District', 'Ward'), selected = 'District'),
                                       selectInput(inputId = 'edge', label = 'Choose Edges', choices = c('Crime Type', 'Location'), selected = 'Crime Type'),
                                       checkboxGroupInput("crimetypes", "Select Crime Types to Include:",
                                                          choiceNames = all.primary.types,
                                                          choiceValues = all.primary.types, inline = TRUE, selected = all.primary.types),
                                       #sliderInput(inputId = 'weightrange', label = 'Select Weight Range', min = 0, max = 100000),
                                       dateRangeInput(inputId = 'daterange', label = 'Choose a Date Range', start = '2017-01-01', end = '2017-12-31', startview = 'month', separator = ' to '),
                                       uiOutput('moreControls'),
                                       sliderInput(inputId = 'binwidth', label = 'Choose Binwidth' , min = 25, max = 250000, value = 250000),
                                       tableOutput('centralitystats'),
                                       br(),
                                       selectInput(inputId = 'centrality', label = 'Select Centrality Measure', choices = c('Weighted Degree Centrality', 'Closeness Centrality', 'Betweenness Centrality', 'Weighted Eigenvector Centrality'), selected = 'Weighted Degree Centrality'),
                                       tableOutput("network.descriptives")),
                                     mainPanel(  
                                       visNetwork::visNetworkOutput('graph'),
                                       plotOutput('strengthdist'),
                                       tableOutput("topcentralities")
                                     )
                                     
                                   )),
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
         h1("Advanced Analytics: Crime Optimization"),
         br(),
         p("Now you know what the data looks like, you might be interested 
                      in improving the crime rates of your ward. Using this tool, 
                      you can tweak some of the network values of a Ward of your 
                      interest and see what effect this might have on the number 
                      of crimes of a certain type in your Ward. This will allow you to see 
                      what you need to improve about your Ward of interest in order 
                      to lower the number of crimes of a specific type."),
         fluidPage(
           tabsetPanel(
             tabPanel("How To Guide",
                      titlePanel("How To Predict Crime Occurence"),
                      p("This page serves as an explanation of what happens 
                                   on the next page and what your possibilities are 
                                   for optimizing your ward. Below, we will run you 
                                   through the networ analytic analysis and how 
                                   this analysis will be used to predict crime 
                                   occurence on the next page."),
                      hr(),
                      h3("1. Approach"),
                      p("In order to predict the overall occurrence of
                                   different crime types by each Chicago Ward,
                                   we will build a 
                                   multilayerd network, that is based on the Crime 
                                   Type--Ward weighted network and exteded with 
                                   data on Police Stations, 311 Service Calls 
                                   (citizen complaints to the municipality about 
                                   a topic in their area), geographic neighbors and 
                                   demographics.
                                   Next, we will regress the edge weight of each edge 
                                   in the weighted Crime Type--Ward network (crime 
                                   occurrence by Ward by Primary Type) on the other 
                                   edges connected to each Ward, thus repeating 
                                   the regression for each crime type, based on the 
                                   data of the 50 wards. Then, we use the 
                                   regression coefficients of all variables by 
                                   each crime type to estimate the total number of 
                                   crimes in a year based on interactively tweaked 
                                   predictor values."),
                      h3("2. Build the Network"),
                      p("A schematic overview of our multilayered network 
                                   is given below. The network is based weighted 
                                   Crime Type--Ward network and extended by:"),
                      tags$ol(
                        tags$li(strong("Ward demographics.")," Information on demographics 
                                   by ward from the US Census, more specifically, 
                                   the distribution of Race ('% White', '% Black' 
                                   & '% Asian'), the percentage of people with 
                                   Hispanic ethnicity and the average income level 
                                   of each working inhabitant. This is not specfically 
                                   network data and will therefore not show up in 
                                   the visualisation."),
                        tags$li(strong("311 Service Call Requests."), " The number of 
                                   requests made to the municipality on the following 
                                   topics: notifications of Abandoned Vehicles, 
                                   requests for new Garbage Carts for household waste, 
                                   Rodent Baiting and rat complaints, Sanitation 
                                   Code Complaints (litter on the street), requests 
                                   for Graffiti Removal and notifications of 
                                   Abandoned Buildings. Each of these topics will 
                                   have its own node and the edge with Ward will be 
                                   weighted based on the number of requests made within 
                                   the boudaries of that ward."),
                        tags$li(strong("Police Stations."), " In Chicago, police stations 
                                   are assigned to police districts, so that almost 
                                   all districs have one police station in them. 
                                   However, Wards do not fit within certain police 
                                   districts and will therefore be connected to a 
                                   police station if it has geographic overlap with 
                                   a district that contains a police station. Hence, 
                                   some wards are connected to 1 station, others to 7 
                                   and, as this is a network, Ward could be connected 
                                   to the same stations. We will include the number 
                                   of edges (number of connected stations) in the 
                                   regression analysis."),
                        tags$li(strong("Geographic Neigbors."), " Wards are connected to 
                                   each other if they share a geographic border. 
                                   In order to include the influence of a broader 
                                   area around the Ward on the occurrence of crimes, 
                                   we include the average values 
                                   of all previously mentioned variables of the 
                                   neighbors of a specific ward in the geospatial 
                                   network. For example, if Ward 2 shares a border 
                                   with only Ward 4 and 17, we include the average 
                                   values of the demographics, 311 Service Requests 
                                   and Police Stations of Ward 4 and 17 as predictors 
                                   in the regression. Thus, each variable will occur 
                                   twice, once for the Ward in question and once as 
                                   the average of the neighbors of the Ward in question."),
                      ),
                      p("Now, based on this network data we will run 
                                   a linear regression, which will be futher elaborated 
                                   on below. Hereafter, we will provide a schematic 
                                   representation of the network and a plot of the 
                                   actual network. For clarity, we did not plot the 
                                   edge weights in the visualisation."),
                      HTML('<center><img src="Schematic_Network.jpg" height="250" width="375"></center>'),
                      h1(strong("[INCLUDE NETWORK VISUALISATION]")),
                      h3("3. Regression Analysis"),
                      p("Now we have a the network in place, we can perform 
                                   a linear regression using the number of crimes 
                                   by each primary type for each ward and, as 
                                   regression predictors, the network edges for 
                                   each ward and ward demographics. This will lead 
                                   to the following regression model: "),
                      HTML('<center><img src="Regression_Model.jpg" height="147" width="490"></center>'),
                      p("We have estimated the regression model and will 
                                   present the results below. Then we will use the 
                                   regression coeffiecients for each crime type to 
                                   predict crime levels on the next page. There you 
                                   will be able to tweak the variable inputs of your 
                                   Ward to see the effects of, for example, increasing 
                                   average income levels in that ward, or buidling 
                                   and additional police station. Keep in mind that 
                                   you will be tweaking the default values of your 
                                   Ward of interest and that the situation in the 
                                   neighboring Wards stay the same. Thus, the values 
                                   for the neighbor variables shall not change. Lastly, 
                                   we will provide a sneak peak of what the data looks like 
                                   that is used in the regression analysis."),
                      DT::DTOutput('regression.coefficients'),
                      hr(),
                      h3("Data Sneak Peak"),
                      p("To restrict the number of columns in the table, 
                                 we have only included the vvalues of the varibales 
                                   that the model will be regressed on."),
                      DT::DTOutput('regression.data'),
             ),
             tabPanel("Crime Prediction",
                      titlePanel("Predicting Spatio Crime Occurrence"),
                      p("Whether crime rates are high or low, it is always 
                                   preferable to decrease crime rates, but what 
                                   effects of your interventions are is quite hard 
                                   to find out. Luckily, using Crimino, you can 
                                   predict the crime rate of your Ward, based on 
                                   tweaks you make to the characteristics of your 
                                   Ward. Below, you can tweak the input values of 
                                   your Ward and see what the predicted number of 
                                   crimes will be for that Ward, based on the new 
                                   situation. Keep in mind that you will be tweaking
                                   the default values of your 
                                   Ward of interest and that the situation in the 
                                   neighboring Wards stay the same. Thus, the values 
                                   for the neighbor variables shall not change."),
                      p("Keep in mind that you will be tweaking the default values of your 
                                   Ward of interest and that the situation in the 
                                   neighboring Wards stay the same. Thus, the values 
                                   for the neighbor variables shall not change."),
                      hr(),
                      useShinyjs(),
                      h3("1. Choose Default Ward Values"),
                      p("Choose a Ward you would like to optimize. You 
                                   can edit the individual Ward characteristics 
                                   below, but note that the average neigbor values 
                                   of this ward will be taken into account for 
                                   calculating the predicted crimes."),
                      fluidRow(
                        box(
                          width = 4, solidHeader = TRUE, status = "primary",
                          sliderInput(inputId = 'ward.input', label = 'Select original ward', min = 1, max = 50, value = 24),
                        ),
                      ),
                      hr(),
                      h3("2. Adjust Inputs"),
                      p("Adjust the inputs for your ward."),
                      fluidRow(
                        shinydashboard::box(
                          title = "Demographics", width = 4, solidHeader = TRUE, status = "primary",
                          uiOutput("income.input"),
                          uiOutput("race.white.input"),
                          uiOutput("race.black.input"),
                          uiOutput("race.asian.input"),
                          uiOutput("ethnicity.input"),
                        ),
                        box(
                          title = "NÂº of 311 Service Calls", width = 4, solidHeader = TRUE, status = "primary",
                          uiOutput("service.calls.input"),
                        ),
                        box(
                          title = "NÂº of Police Stations", width = 4, solidHeader = TRUE, status = "primary",
                          uiOutput("police.input"),
                        ),
                      ),
                      hr(),
                      h3("Prediction of number of crimes"),
                      p("The table below will show the predicted crime 
                                   rates for each crime type, based on your inputs 
                                   above. A negative predicted value of crimes 
                                   will show in the table as no crimes expected. 
                                   Some crime type only occur very rarely and 
                                   not even in each Ward, hence the potential for 
                                   negative values. As the negativity of the rate 
                                   will only indicate the likelihood that a crime 
                                   will not occur, it should be fine to show them 
                                   as zero for this exercise"),
                      DT::DTOutput('predictions')
             )
                      )
                    )
)
)

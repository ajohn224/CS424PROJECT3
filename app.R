#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinydashboard)
library(tigris)
library(mapview)
library(dplyr)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(data.table)
library(ggplot2)
library(DT)
library(stringr)




data <- read.csv(file = "energy-usage-2010.csv",fill = TRUE, sep = ",", header = TRUE,stringsAsFactors=FALSE )

#names(data)[names(data) == "CENSUS.BLOCK"] <- "GEOID10"
names(data)[names(data) == "COMMUNITY.AREA.NAME"] <- "COMMUNITYAREA"
names(data)[names(data) == "BUILDING.TYPE"] <- "TYPE"
names(data)[names(data) == "BUILDING_SUBTYPE"] <- "SUBTYPE"
names(data)[names(data) == "KWH.JANUARY.2010"] <- "KWH_JAN"
names(data)[names(data) == "KWH.FEBRUARY.2010"] <- "KWH_FEB"
names(data)[names(data) == "KWH.MARCH.2010"] <- "KWH_MARCH"
names(data)[names(data) == "KWH.APRIL.2010"] <- "KWH_APRIL"
names(data)[names(data) == "KWH.MAY.2010"] <- "KWH_MAY"
names(data)[names(data) == "KWH.JUNE.2010"] <- "KWH_JUNE"
names(data)[names(data) == "KWH.JULY.2010"] <- "KWH_JULY"
names(data)[names(data) == "KWH.AUGUST.2010"] <- "KWH_AUGUST"
names(data)[names(data) == "KWH.SEPTEMBER.2010"] <- "KWH_SEPTEMBER"
names(data)[names(data) == "KWH.OCTOBER.2010"] <- "KWH_OCTOBER"
names(data)[names(data) == "KWH.NOVEMBER.2010"] <- "KWH_NOVEMBER"
names(data)[names(data) == "KWH.DECEMBER.2010"] <- "KWH_DECEMBER"
names(data)[names(data) == "THERM.JANUARY.2010"] <- "THERM_JAN"
names(data)[names(data) == "THERM.FEBRUARY.2010"] <- "THERM_FEB"
names(data)[names(data) == "THERM.MARCH.2010"] <- "THERM_MARCH"
names(data)[names(data) == "TERM.APRIL.2010"] <- "THERM_APRIL"
names(data)[names(data) == "THERM.MAY.2010"] <- "THERM_MAY"
names(data)[names(data) == "THERM.JUNE.2010"] <- "THERM_JUNE"
names(data)[names(data) == "THERM.JULY.2010"] <- "THERM_JULY"
names(data)[names(data) == "THERM.AUGUST.2010"] <- "THERM_AUGUST"
names(data)[names(data) == "THERM.SEPTEMBER.2010"] <- "THERM_SEPTEMBER"
names(data)[names(data) == "THERM.OCTOBER.2010"] <- "THERM_OCTOBER"
names(data)[names(data) == "THERM.NOVEMBER.2010"] <- "THERM_NOVEMBER"
names(data)[names(data) == "THERM.DECEMBER.2010"] <- "THERM_DECEMBER"
names(data)[names(data) == "TOTAL.KWH"] <- "TOTAL_KWH"
names(data)[names(data) == "TOTAL.THERMS"] <- "TOTAL_THERMS"
names(data)[names(data) == "TOTAL.POPULATION"] <- "TOTAL_POPULATION"
names(data)[names(data) == "AVERAGE.BUILDING.AGE"] <- "BUILDING_AGE"
names(data)[names(data) == "AVERAGE.STORIES"] <- "BUILDING_HEIGHT"
data$CENSUS.BLOCK = as.character(data$CENSUS.BLOCK)
data = subset(data,data$CENSUS.BLOCK!=" ")
data2 <- subset(data,select=c("CENSUS.BLOCK","COMMUNITYAREA","TYPE","SUBTYPE","KWH_JAN","KWH_FEB","KWH_MARCH","KWH_APRIL",
                              "KWH_MAY","KWH_JUNE","KWH_JULY","KWH_AUGUST","KWH_SEPTEMBER","KWH_OCTOBER","KWH_NOVEMBER",
                              "KWH_DECEMBER", "THERM_JAN","THERM_FEB", "THERM_MARCH","THERM_APRIL","THERM_MAY","THERM_JUNE",
                              "THERM_JULY","THERM_AUGUST","THERM_SEPTEMBER","THERM_OCTOBER","THERM_NOVEMBER","THERM_DECEMBER",
                              "TOTAL_KWH","TOTAL_THERMS","TOTAL_POPULATION","BUILDING_AGE","BUILDING_HEIGHT"))
data3 <- subset(data,select=c("CENSUS.BLOCK","COMMUNITYAREA","TYPE","SUBTYPE","KWH_JAN","KWH_FEB","KWH_MARCH","KWH_APRIL",
                              "KWH_MAY","KWH_JUNE","KWH_JULY","KWH_AUGUST","KWH_SEPTEMBER","KWH_OCTOBER","KWH_NOVEMBER",
                              "KWH_DECEMBER", "THERM_JAN","THERM_FEB", "THERM_MARCH","THERM_APRIL","THERM_MAY","THERM_JUNE",
                              "THERM_JULY","THERM_AUGUST","THERM_SEPTEMBER","THERM_OCTOBER","THERM_NOVEMBER","THERM_DECEMBER",
                              "TOTAL_KWH","TOTAL_THERMS","TOTAL_POPULATION","BUILDING_AGE","BUILDING_HEIGHT"))

names(data2)[names(data2) == "CENSUS.BLOCK"] <- "GEOID10"

names(data3)[names(data3) == "CENSUS.BLOCK"] <- "GEOID"
data3$GEOID <- strtrim(data3$GEOID, 11)
blocksData <- blocks(state = "IL", county = "Cook")
tractsData <- tracts(state = "IL", county = "Cook")
joinedData <-merge(x= blocksData,y=data2,by="GEOID10",all= FALSE)
joinedData2 <-merge(x= blocksData,y=data2,by="GEOID10",all= FALSE)
joinedData3 <-merge(x= tractsData,y=data3,by="GEOID",all= FALSE)

nwsjoinedData <- subset(joinedData,joinedData$COMMUNITYAREA == "Near West Side")
nwsData2 <- subset(data2,data2$COMMUNITYAREA == "Near West Side")
options(tigris_use_cache = TRUE)
community_List <- c(unique(joinedData[[c("COMMUNITYAREA")]]))

part3_List <-c(
  "oldest buildings",
  "newest building",
  "tallest buildings",
  "most electricity over the year",
  "most gas over the year",
  "most population",
  "most occupied percentage",
  "highest percentage of renters"
)
data_List <- c(
  "Electricity",
  "Gas",
  "Building Age",
  "Building Type",
  "Building Height",
  "Total Population"
  
)
selection_List <-c(
  "All Months",
  "Specific Month"
)
legend_List <-c(
  "mapviewVectorColors",
  "mapviewTopoColors",
  "mapviewRasterColors"
  
)
months_List <-c(
  "All Months",
  "January",
  "February",
  "March",
  "April",
  "May",
  "June",
  "July",
  "August",
  "September",
  "October",
  "November",
  "December"
)

type_List <-c (
  "All Types",
  "Residential",
  "Commercial",
  "Industrial"
)
ui<- fluidPage(
    
    tags$head(
        tags$style(HTML("
      .selectize-input {
        height: 50px;
        width: 150px;
        font-size: 8pt;
        padding-top: 3px;
      padding-down: 3px;

      }
    ")
                   
        )
        #,
    #     tags$style(HTML("
    #   .selectize-boxInput {
    #     height: 50px;
    #     width: 150px;
    #     font-size: 8pt;
    #     padding-top: 3px;
    #   padding-down: 3px;
    # 
    #   }
    # ")
    #                
    #     )
        
    ),
    dashboardPage(
        dashboardHeader(title = "CS 424 Spring 2021 Project 3"),
        
        dashboardSidebar (disable = FALSE, collapsed = FALSE,
                          
                          sidebarMenu(
                              menuItem("Near West Side Community Area", tabName = "NWSC", icon =NULL),
                              menuItem("Community Comparisons", tabName = "communitycomparison", icon =icon("dashboard")),
                              menuItem("Chicago", tabName = "chicago", icon =icon("dashboard")),
                              menuItem("About", tabName = "about", icon = NULL)
                          )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName ="NWSC",
                        
                        fluidRow(
                            column(12,
                                   fluidRow(shinydashboard::box(title = "Map of the Near West Side Community Area.", solidHeader = TRUE, 
                                                status = "primary",width = 150,
                                                column(1,selectInput('selection', 'Select Data:',
                                                                     choices=data_List,selected = "Electricity"),
                                                       
                                                       conditionalPanel(
                                                         condition = "input.selection == 'Electricity'",
                                                         selectInput(
                                                           "kwhmonthSelection", "Select Month",
                                                           choices = months_List
                                                         )),
                                                       conditionalPanel(
                                                         condition = "input.selection == 'Gas'",
                                                         selectInput(
                                                           "gasmonthSelection", "Select Month",
                                                           choices = months_List
                                                         )),
                                                       conditionalPanel(
                                                         condition = "input.selection == 'Building Type'",
                                                         selectInput(
                                                           "typeSelection", "Select Building Type",
                                                           choices = type_List
                                                         ))
                                                    
                                                       
                                                       
                                                       )
                                                ,
                                                column(11, leafletOutput("nwsmap",height = 450))
                                            
                                  
                                   )
                                   
                            )
                            
                            
                        )
                        
                ),
                
                             column(6,
                                    fluidRow(
                                      shinydashboard::box(title = "", solidHeader = TRUE, status = "primary", width =20,
                                          DT::dataTableOutput("nwsTable",height=200)
                                      )))
                
                             # column(6,
                             #        fluidRow(
                             #          shinydashboard::box(title = "", solidHeader = TRUE, status = "primary", width =20
                             #          )))
                             
                             
                             
                             
                             
                         
                ),
               
                tabItem(tabName="communitycomparison",
                        
                        fluidRow(
                          column(6,
                                 fluidRow(shinydashboard::box(title = textOutput("title1"), solidHeader = TRUE, 
                                              status = "primary",width = 100,
                                              column(3,selectInput('selection1', 'Select Data:',
                                                                   choices=data_List,selected = "Electricity"),
                                                     selectInput('communityselection', 'Select Community Area:',
                                                                 choices=community_List,selected = "Near West Side"),
                                                     selectInput('legendselection', 'Select Legend Color:',
                                                                 choices=legend_List,selected = "mapviewVectorColors"),
                                                     conditionalPanel(
                                                       condition = "input.selection1 == 'Electricity'",
                                                       selectInput(
                                                         "kwhmonthSelection1", "Select Month",
                                                         choices = months_List
                                                       )),
                                                     conditionalPanel(
                                                       condition = "input.selection1 == 'Gas'",
                                                       selectInput(
                                                         "gasmonthSelection1", "Select Month",
                                                         choices = months_List
                                                       )),
                                                     conditionalPanel(
                                                       condition = "input.selection1 == 'Building Type'",
                                                       selectInput(
                                                         "typeSelection1", "Select Building Type",
                                                         choices = type_List
                                                       )),
                                                     
                                                     
                                                     
                                                     
                                              )
                                               ,
                                               column(9, leafletOutput("choicemap1",height = 350) )
                                              
                                              
                                 )
                                 
                                 )
                                 
                                 
                          ),
                          column(6,
                                 fluidRow(shinydashboard::box(title = textOutput("title2"), solidHeader = TRUE, 
                                              status = "primary",width = 100,
                                              column(3,selectInput('selection2', 'Select Data:',
                                                                   choices=data_List,selected = "Electricity"),
                                                     selectInput('communityselection2', 'Select Community Area:',
                                                                 choices=community_List,selected = "Loop"),
                                                     selectInput('legendselection1', 'Select Legend Color:',
                                                                 choices=legend_List,selected = "mapviewVectorColors"),
                                                     conditionalPanel(
                                                       condition = "input.selection2 == 'Electricity'",
                                                       selectInput(
                                                         "kwhmonthSelection2", "Select Month",
                                                         choices = months_List
                                                       )),
                                                     conditionalPanel(
                                                       condition = "input.selection2 == 'Gas'",
                                                       selectInput(
                                                         "gasmonthSelection2", "Select Month",
                                                         choices = months_List
                                                       )),
                                                     conditionalPanel(
                                                       condition = "input.selection2 == 'Building Type'",
                                                       selectInput(
                                                         "typeSelection2", "Select Building Type",
                                                         choices = type_List
                                                       ))
                                                     
                                                     
                                                     
                                              )
                                              ,
                                              column(9,leafletOutput("choicemap2",height = 350))

                                              
                                 )
                                 
                                 )
                                 
                                 
                          )

                        ),
                        column(6,
                               fluidRow(
                                 shinydashboard::box(title = "", solidHeader = TRUE, status = "primary", width =20,
                                                     DT::dataTableOutput("part2Table",height=200)
                                 ))),
                        column(6,
                               fluidRow(
                                 shinydashboard::box(title = "", solidHeader = TRUE, status = "primary", width =20,
                                                     DT::dataTableOutput("part2Table1",height=200)
                                                     
                                 )))
                   
                     
                ),
                tabItem(tabName="chicago",
                        
                        fluidRow(
                          column(12,
                                 fluidRow(shinydashboard::box(title = "Map of Chicago.", solidHeader = TRUE, 
                                                              status = "primary",width = 150,
                                                              column(1,selectInput('selection3', 'Select Data:',
                                                                                   choices=data_List,selected = "Electricity"),
                                                                     selectInput('communityselection3', 'Select Community Area:',
                                                                                 choices=community_List,selected = "Loop"),
                                                                     selectInput('legendselection3', 'Select Legend Color:',
                                                                                 choices=legend_List,selected = "mapviewVectorColors"),
                                                                     selectInput('part3selection', 'Select 10% Census Tract Option:',
                                                                                 choices=part3_List,selected = "oldest buildings"),
                                                                     conditionalPanel(
                                                                       condition = "input.selection3 == 'Electricity'",
                                                                       selectInput(
                                                                         "kwhmonthSelection3", "Select Month",
                                                                         choices = months_List
                                                                       )),
                                                                     conditionalPanel(
                                                                       condition = "input.selection3 == 'Gas'",
                                                                       selectInput(
                                                                         "gasmonthSelection3", "Select Month",
                                                                         choices = months_List
                                                                       )),
                                                                     conditionalPanel(
                                                                       condition = "input.selection3 == 'Building Type'",
                                                                       selectInput(
                                                                         "typeSelection3", "Select Building Type",
                                                                         choices = type_List
                                                                       ))
                                                                     
                                                                     
                                                                     
                                                              )
                                                              ,
                                                              column(11, leafletOutput("chicagomap",height = 450))
                                                              
                                                              
                                 )
                                 
                                 )
                                 
                                 
                          )
                          
                        )
                ),
                tabItem(tabName="about",
                        
                        "This project was done by AmalJosy Johnson on 4/24/2021."
              
                        
                        
                        
                )
                
            )
            
        )
            )
        
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$title1 <- renderText(paste("Map of", input$communityselection ))
  output$title2 <- renderText(paste("Map of", input$communityselection2 ))
  
  # 
  # myReactiveFunc <- reactive({
  #   toReturn <- NULL
  #   if(input$All){
  #     toReturn <- nwsjoinedData
  #   }
  #   else {
  #     if(input$January){
  #     toReturn <- rbind(toReturn,subset(nwsjoinedData,select=c("KWH_JAN")))
  #    }
  #   }
  # })
  
    output$nwsmap <- renderLeaflet({
      
       # reactiveFunc <- myReactiveFunc()
        if(input$selection == "Electricity"){
        
          if(input$kwhmonthSelection == "All Months"){
            
            mapview(nwsjoinedData,layer.name = 'Total Electricity ',zcol="TOTAL_KWH")@map%>%
            addResetMapButton()
            
          }
          
          else if(input$kwhmonthSelection == "January"){
            mapview(nwsjoinedData,layer.name = 'Electricity in January',zcol="KWH_JAN")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "February"){
            mapview(nwsjoinedData,layer.name = 'Electricity in February',zcol="KWH_FEB")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "March"){
            mapview(nwsjoinedData,layer.name = 'Electricity in March',zcol="KWH_MARCH")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "April"){
            mapview(nwsjoinedData,layer.name = 'Electricity in April',zcol="KWH_APRIL")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "May"){
            mapview(nwsjoinedData,layer.name = 'Electricity in May',zcol="KWH_MAY")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "June"){
            mapview(nwsjoinedData,layer.name = 'Electricity in June',zcol="KWH_JUNE")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "July"){
            mapview(nwsjoinedData,layer.name = 'Electricity in July',zcol="KWH_JULY")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "August"){
            mapview(nwsjoinedData,layer.name = 'Electricity in August',zcol="KWH_AUGUST")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "September"){
            mapview(nwsjoinedData,layer.name = 'Electricity in September',zcol="KWH_SEPTEMBER")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "October"){
            mapview(nwsjoinedData,layer.name = 'Electricity in October',zcol="KWH_OCTOBER")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "November"){
            mapview(nwsjoinedData,layer.name = 'Electricity in November',zcol="KWH_NOVEMBER")@map%>%
              addResetMapButton()
            
          }
          else if(input$kwhmonthSelection == "December"){
            mapview(nwsjoinedData,layer.name = 'Electricity in December',zcol="KWH_DECEMBER")@map%>%
              addResetMapButton()
            
          }
          
        }
        else if (input$selection == "Gas"){

            if(input$gasmonthSelection == "All Months"){
              
              mapview(nwsjoinedData,layer.name = 'Total Gas ',zcol="TOTAL_THERMS")@map%>%
                addResetMapButton()
              
            }
            
            else if(input$gasmonthSelection == "January"){
              mapview(nwsjoinedData,layer.name = 'Gas in January',zcol="THERM_JAN")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "February"){
              mapview(nwsjoinedData,layer.name = 'Gas in February',zcol="THERM_FEB")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "March"){
              mapview(nwsjoinedData,layer.name = 'Gas in March',zcol="THERM_MARCH")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "April"){
              mapview(nwsjoinedData,layer.name = 'Gas in April',zcol="THERM_APRIL")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "May"){
              mapview(nwsjoinedData,layer.name = 'Gas in May',zcol="THERM_MAY")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "June"){
              mapview(nwsjoinedData,layer.name = 'Gas in June',zcol="THERM_JUNE")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "July"){
              mapview(nwsjoinedData,layer.name = 'Gas in July',zcol="THERM_JULY")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "August"){
              mapview(nwsjoinedData,layer.name = 'Gas in August',zcol="THERM_AUGUST")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "September"){
              mapview(nwsjoinedData,layer.name = 'Gas in September',zcol="THERM_SEPTEMBER")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "October"){
              mapview(nwsjoinedData,layer.name = 'Gas in October',zcol="THERM_OCTOBER")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "November"){
              mapview(nwsjoinedData,layer.name = 'Gas in November',zcol="THERM_NOVEMBER")@map%>%
                addResetMapButton()
              
            }
            else if(input$gasmonthSelection == "December"){
              mapview(nwsjoinedData,layer.name = 'Gas in December',zcol="THERM_DECEMBER")@map%>%
                addResetMapButton()
              
            }
        }
      else if(input$selection == "Building Age"){
        mapview(nwsjoinedData,layer.name = 'Building Age',zcol="BUILDING_AGE")@map%>%
          addResetMapButton()
      }
      else if(input$selection == "Building Type"){
        if(input$typeSelection== "All Types"){
          mapview(nwsjoinedData,layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection== "Residential"){
          nwsresidentialData <- subset(nwsjoinedData,nwsjoinedData$TYPE =="Residential")
          mapview(nwsresidentialData,layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection== "Commercial"){
          nwscommerciallData <- subset(nwsjoinedData,nwsjoinedData$TYPE =="Commercial")
          mapview(nwscommerciallData,layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection== "Industrial"){
          nwsindustrialData <- subset(nwsjoinedData,nwsjoinedData$TYPE =="Industrial")
          mapview(nwsindustrialData,layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
       
      }
      else if(input$selection == "Building Height"){
        mapview(nwsjoinedData,layer.name = 'Building Height',zcol="BUILDING_HEIGHT")@map%>%
          addResetMapButton()
      }
      else if(input$selection == "Total Population"){
        mapview(nwsjoinedData,layer.name = 'Total Population',zcol="TOTAL_POPULATION")@map%>%
          addResetMapButton()
      }
    })
    output$choicemap1 <- renderLeaflet({
      pal = mapviewPalette(input$legendselection)
      
      # reactiveFunc <- myReactiveFunc()
      if(input$selection1 == "Electricity"){

        if(input$kwhmonthSelection1 == "All Months"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Total Electricity ',zcol="TOTAL_KWH")@map%>%
            addResetMapButton()
          
        }
        
        else if(input$kwhmonthSelection1 == "January"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in January',zcol="KWH_JAN")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "February"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in February',zcol="KWH_FEB")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "March"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in March',zcol="KWH_MARCH")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "April"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in April',zcol="KWH_APRIL")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "May"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in May',zcol="KWH_MAY")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "June"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in June',zcol="KWH_JUNE")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "July"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in July',zcol="KWH_JULY")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "August"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in August',zcol="KWH_AUGUST")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "September"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in September',zcol="KWH_SEPTEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "October"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in October',zcol="KWH_OCTOBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "November"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in November',zcol="KWH_NOVEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection1 == "December"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Electricity in December',zcol="KWH_DECEMBER")@map%>%
            addResetMapButton()
          
        }
        
      }
      else if (input$selection1 == "Gas"){
        
        if(input$gasmonthSelection1 == "All Months"){
          
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Total Gas ',zcol="TOTAL_THERMS")@map%>%
            addResetMapButton()
          
        }
        
        else if(input$gasmonthSelection1 == "January"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in January',zcol="THERM_JAN")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "February"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in February',zcol="THERM_FEB")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "March"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in March',zcol="THERM_MARCH")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "April"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in April',zcol="THERM_APRIL")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "May"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in May',zcol="THERM_MAY")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "June"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in June',zcol="THERM_JUNE")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "July"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in July',zcol="THERM_JULY")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "August"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in August',zcol="THERM_AUGUST")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "September"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in September',zcol="THERM_SEPTEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "October"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in October',zcol="THERM_OCTOBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "November"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in November',zcol="THERM_NOVEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection1 == "December"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Gas in December',zcol="THERM_DECEMBER")@map%>%
            addResetMapButton()
          
        }
      }
      else if(input$selection1 == "Building Age"){
        mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Building Age',zcol="BUILDING_AGE")@map%>%
          addResetMapButton()
      }
      else if(input$selection1 == "Building Type"){
        if(input$typeSelection1== "All Types"){
          mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection1== "Residential"){
          communityData <- subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Residential")
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection1== "Commercial"){
          communityData <- subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Commercial")          
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection1== "Industrial"){
          communityData <- subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Industrial") 
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'Building Type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        
      }
      else if(input$selection1 == "Building Height"){
        mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Building Height',zcol="BUILDING_HEIGHT")@map%>%
          addResetMapButton()
      }
      else if(input$selection1 == "Total Population"){
        mapview(subset(joinedData,joinedData$COMMUNITYAREA == input$communityselection),col.regions = pal(100),layer.name = 'Total Population',zcol="TOTAL_POPULATION")@map%>%
          addResetMapButton()
      }
    })
    output$choicemap2 <- renderLeaflet({
      pal = mapviewPalette(input$legendselection1)
      
      joinedData4 <- subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2)
      # reactiveFunc <- myReactiveFunc()
      if(input$selection2 == "Electricity"){
        
        if(input$kwhmonthSelection2 == "All Months"){
          
          mapview(joinedData4,col.regions = pal(100),layer.name = 'Total electricity ',zcol="TOTAL_KWH")@map%>%
            addResetMapButton()
          
        }
      
        
        else if(input$kwhmonthSelection2 == "January"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in january',zcol="KWH_JAN")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "February"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in february',zcol="KWH_FEB")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "March"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in march',zcol="KWH_MARCH")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "April"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in april',zcol="KWH_APRIL")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "May"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in may',zcol="KWH_MAY")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "June"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in june',zcol="KWH_JUNE")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "July"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in july',zcol="KWH_JULY")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "August"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in august',zcol="KWH_AUGUST")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "September"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in september',zcol="KWH_SEPTEMBER")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "October"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in october',zcol="KWH_OCTOBER")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "November"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in november',zcol="KWH_NOVEMBER")@map%>%
            addResetMapButton()

        }
        else if(input$kwhmonthSelection2 == "December"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Electricity in december',zcol="KWH_DECEMBER")@map%>%
            addResetMapButton()

        }

      }
      else if (input$selection2 == "Gas"){

        if(input$gasmonthSelection2 == "All Months"){

          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Total gas ',zcol="TOTAL_THERMS")@map%>%
            addResetMapButton()

        }

        else if(input$gasmonthSelection2 == "January"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in january',zcol="THERM_JAN")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "February"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in february',zcol="THERM_FEB")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "March"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in march',zcol="THERM_MARCH")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "April"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in april',zcol="THERM_APRIL")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "May"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in may',zcol="THERM_MAY")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "June"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in june',zcol="THERM_JUNE")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "July"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in july',zcol="THERM_JULY")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "August"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in august',zcol="THERM_AUGUST")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "September"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in september',zcol="THERM_SEPTEMBER")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "October"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in october',zcol="THERM_OCTOBER")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "November"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in november',zcol="THERM_NOVEMBER")@map%>%
            addResetMapButton()

        }
        else if(input$gasmonthSelection2 == "December"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Gas in december',zcol="THERM_DECEMBER")@map%>%
            addResetMapButton()

        }
      }
      else if(input$selection2 == "Building Age"){
        mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Building age',zcol="BUILDING_AGE")@map%>%
          addResetMapButton()
      }
      else if(input$selection2 == "Building Type"){
        if(input$typeSelection2== "All Types"){
          mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection2== "Residential"){
          communityData <- subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Residential")
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'Building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection2== "Commercial"){
          communityData <- subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Commercial")
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'Building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection2== "Industrial"){
          communityData <- subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Industrial")
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'Building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }

      }
      else if(input$selection2 == "Building Height"){
        mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Building height',zcol="BUILDING_HEIGHT")@map%>%
          addResetMapButton()
      }
      else if(input$selection2 == "Total Population"){
        mapview(subset(joinedData2,joinedData2$COMMUNITYAREA == input$communityselection2),col.regions = pal(100),layer.name = 'Total population',zcol="TOTAL_POPULATION")@map%>%
          addResetMapButton()
      }
    })
    output$nwsTable <- DT::renderDataTable(
      if(input$selection == "Gas"){
      if(input$gasmonthSelection == "All Months"){
        
        DT::datatable({ 
          data <- data.table(nwsData2[,c("TOTAL_THERMS","TOTAL_KWH")])

        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      
    
      
     else if(input$gasmonthSelection == "January"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
        
     }
 
      else if(input$gasmonthSelection == "February"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_FEB","KWH_FEB")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
  
      else if(input$gasmonthSelection == "March"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_MARCH","KWH_MARCH")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
      }
      else if(input$gasmonthSelection == "April"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "May"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "June"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "July"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "August"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "September"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "October"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      else if(input$gasmonthSelection == "November"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
      }
      else if(input$gasmonthSelection == "December"){
        DT::datatable({ 
          data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
        }, 
        options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
        ), rownames = FALSE 
        )
        
      }
      }
     else if(input$selection == "Electricity"){
       if(input$kwhmonthSelection == "All Months"){
         
         DT::datatable({ 
           data <- data.table(nwsData2[,c("TOTAL_THERMS","TOTAL_KWH")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       
       
       
       else if(input$kwhmonthSelection == "January"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
         
       }
       
       else if(input$kwhmonthSelection == "February"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_FEB","KWH_FEB")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "March"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_MARCH","KWH_MARCH")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
       }
       else if(input$kwhmonthSelection == "April"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "May"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "June"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "July"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "August"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "September"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "October"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
       else if(input$kwhmonthSelection == "November"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
       }
       else if(input$kwhmonthSelection == "December"){
         DT::datatable({ 
           data <- data.table(nwsData2[,c("THERM_JAN","KWH_JAN")])
         }, 
         options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
         ), rownames = FALSE 
         )
         
       }
     }
       
      
     
      
      
    )
    
    output$part2Table <- DT::renderDataTable(

      if(input$selection1 == "Gas"){
        if(input$gasmonthSelection1 == "All Months"){
          
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("TOTAL_THERMS","TOTAL_KWH")])
            
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        
        
        
        else if(input$gasmonthSelection1 == "January"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
          
        }
        
        else if(input$gasmonthSelection1 == "February"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_FEB","KWH_FEB")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        
        else if(input$gasmonthSelection1 == "March"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_MARCH","KWH_MARCH")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$gasmonthSelection1 == "April"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "May"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "June"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "July"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "August"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "September"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "October"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection1 == "November"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$gasmonthSelection1 == "December"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
      }
      else if(input$selection1 == "Electricity"){
        if(input$kwhmonthSelection1 == "All Months"){
          
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("TOTAL_THERMS","TOTAL_KWH")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        
        
        
        else if(input$kwhmonthSelection1 == "January"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
          
        }
        
        else if(input$kwhmonthSelection1 == "February"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_FEB","KWH_FEB")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "March"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_MARCH","KWH_MARCH")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$kwhmonthSelection1 == "April"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_APRIL","KWH_APRIL")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "May"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_MAY","KWH_MAY")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "June"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JUNE","KWH_JUNE")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "July"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_JULY","KWH_JULY")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "August"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_AUGUST","KWH_AUGUST")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "September"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_SEPTEMBER","KWH_SEPTEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "October"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_OCTOBER","KWH_OCTOBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection1 == "November"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_NOVEMBER","KWH_NOVEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$kwhmonthSelection1 == "December"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection)[,c("THERM_DECEMBER","KWH_DECEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
      }
      
      
      
      
      
    )
    output$part2Table1 <- DT::renderDataTable(
      if(input$selection2 == "Gas"){
        if(input$gasmonthSelection2 == "All Months"){
          
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("TOTAL_THERMS","TOTAL_KWH")])
            
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        
        
        
        else if(input$gasmonthSelection2 == "January"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
          
        }
        
        else if(input$gasmonthSelection2 == "February"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_FEB","KWH_FEB")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        
        else if(input$gasmonthSelection2 == "March"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_MARCH","KWH_MARCH")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$gasmonthSelection2 == "April"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_APRIL","KWH_APRIL")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "May"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_MAY","KWH_MAY")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "June"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_JUNE","KWH_JUNE")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "July"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_JULY","KWH_JULY")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "August"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_AUGUST","KWH_AUGUST")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "September"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_SEPTEMBER","KWH_SEPTEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "October"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_OCOTBER","KWH_OCOTBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$gasmonthSelection2 == "November"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_NOVEMBER","KWH_NOVEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$gasmonthSelection2 == "December"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_DECEMBER","KWH_DECEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
      }
      else if(input$selection2 == "Electricity"){
        if(input$kwhmonthSelection2 == "All Months"){
          
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("TOTAL_THERMS","TOTAL_KWH")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        
        
        
        else if(input$kwhmonthSelection2 == "January"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_JAN","KWH_JAN")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
          
        }
        
        else if(input$kwhmonthSelection2 == "February"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_FEB","KWH_FEB")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "March"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_MARCH","KWH_MARCH")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$kwhmonthSelection2 == "April"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_APRIL","KWH_APRIL")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "May"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_MAY","KWH_MAY")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "June"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_JUNE","KWH_JUNE")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "July"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_JULY","KWH_JULY")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "August"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_AUGUST","KWH_AUGUST")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "September"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_SEPTEMBER","KWH_SEPTEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "October"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_OCTOBER","KWH_OCTOBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
        else if(input$kwhmonthSelection2 == "November"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_NOVEMBER","KWH_NOVEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
        }
        else if(input$kwhmonthSelection2 == "December"){
          DT::datatable({ 
            data <- data.table(subset(data2,data2$COMMUNITYAREA == input$communityselection2)[,c("THERM_DECEMBER","KWH_DECEMBER")])
          }, 
          options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
          ), rownames = FALSE 
          )
          
        }
      }
      
      
      
      
      
    )
    output$chicagomap <- renderLeaflet({
      pal = mapviewPalette(input$legendselection3)
      

      if(input$selection3 == "Electricity"){

        if(input$kwhmonthSelection3 == "All Months"){
          if(input$part3selection=="oldest buildings"){
            n <- 10
            trialdata <-joinedData3[joinedData3$BUILDING_AGE > quantile(joinedData3$BUILDING_AGE,prob=1-n/100),]
            mapview(trialdata,col.regions = pal(100),layer.name = 'total electricity ',zcol="TOTAL_KWH")@map%>%
              addResetMapButton()
          }
          else{
            mapview(joinedData3,col.regions = pal(100),layer.name = 'total electricity ',zcol="TOTAL_KWH")@map%>%
            addResetMapButton()
            }
          
        }
        
        else if(input$kwhmonthSelection3 == "January"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in january',zcol="KWH_JAN")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "February"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in february',zcol="KWH_FEB")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "March"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in march',zcol="KWH_MARCH")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "April"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in april',zcol="KWH_APRIL")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "May"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in may',zcol="KWH_MAY")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "June"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in june',zcol="KWH_JUNE")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "July"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in july',zcol="KWH_JULY")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "August"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in august',zcol="KWH_AUGUST")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "September"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in september',zcol="KWH_SEPTEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "October"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in october',zcol="KWH_OCTOBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "November"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in november',zcol="KWH_NOVEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$kwhmonthSelection3 == "December"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'electricity in december',zcol="KWH_DECEMBER")@map%>%
            addResetMapButton()
          
        }
        
      }
      else if (input$selection3 == "Gas"){
        
        if(input$gasmonthSelection3 == "All Months"){
          
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'total gas ',zcol="TOTAL_THERMS")@map%>%
            addResetMapButton()
          
        }
        
        else if(input$gasmonthSelection3 == "January"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in january',zcol="THERM_JAN")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "February"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in february',zcol="THERM_FEB")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "March"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in march',zcol="THERM_MARCH")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "April"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in april',zcol="THERM_APRIL")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "May"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in may',zcol="THERM_MAY")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "June"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in june',zcol="THERM_JUNE")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "July"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in july',zcol="THERM_JULY")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "August"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in august',zcol="THERM_AUGUST")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "September"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in september',zcol="THERM_SEPTEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "October"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in october',zcol="THERM_OCTOBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "November"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in november',zcol="THERM_NOVEMBER")@map%>%
            addResetMapButton()
          
        }
        else if(input$gasmonthSelection3 == "December"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'gas in december',zcol="THERM_DECEMBER")@map%>%
            addResetMapButton()
          
        }
      }
      else if(input$selection3 == "Building Age"){
        mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'building age',zcol="BUILDING_AGE")@map%>%
          addResetMapButton()
      }
      else if(input$selection3 == "Building Type"){
        if(input$typeSelection3== "All Types"){
          mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection3== "Residential"){
          communityData <- subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Residential")
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection3== "Commercial"){
          communityData <- subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Commercial")          
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        else if(input$typeSelection3== "Industrial"){
          communityData <- subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3)
          communityresidentialData <- subset(communityData,communityData$TYPE =="Industrial") 
          mapview(communityresidentialData,col.regions = pal(100),layer.name = 'building type',zcol="TYPE")@map%>%
            addResetMapButton()
        }
        
      }
      else if(input$selection3 == "Building Height"){
        mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'building height',zcol="BUILDING_HEIGHT")@map%>%
          addResetMapButton()
      }
      else if(input$selection3 == "Total Population"){
        mapview(subset(joinedData3,joinedData3$COMMUNITYAREA == input$communityselection3),col.regions = pal(100),layer.name = 'total population',zcol="TOTAL_POPULATION")@map%>%
          addResetMapButton()
      }
          
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

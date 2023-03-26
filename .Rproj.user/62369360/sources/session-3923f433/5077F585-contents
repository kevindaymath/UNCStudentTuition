options(warn=-1)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)


# Define UI for dataset viewer application

dashboardPage(
  dashboardHeader(title = "Fang Shi Demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map")
    )
  ),
  dashboardBody(
    tabItem(tabName = "dashboard",
            selectInput("season",label="Season",
                        choices=c("Fall"), selected="Fall"),
            selectInput("year",label="Year",
                        choices=c(2018,2017), selected=2018),
    fluidRow(
      
      
      box(width = 12,
        title = "In State Map",
        leafletOutput("Map")
      )
    ),
    dataTableOutput("Table")
    )
    
  )
)
# fluidPage(
#   
#   # Application title
# 
#     
#         
#         
#         
#       )
#       
# 

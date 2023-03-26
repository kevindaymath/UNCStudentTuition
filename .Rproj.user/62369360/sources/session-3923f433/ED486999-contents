options(warn=-1)

# Packages <- c("plyr","dplyr","foreign","shiny", "DT", "leaflet", "datasets", "devtools", "maptools", "rgdal", "rmapshaper", "rgeos","tidyr")
# lapply(Packages, library, character.only = TRUE) # Load above packages
library(dplyr)
load("QGIS/NCShp.RData") # Loads shapefiles
all_data <- read.csv("data/all_data.csv") # Reads in data


function(input, output) {
  # UI Reactives INPUTS
  chosenSeason <- reactive({    # Season input
    input$season
  })
  chosenYear <- reactive({    # Year input
    input$year
  })
  
  # OUTPUTS
  output$Table <- renderDataTable({
    a <- all_data %>%
      filter(Term == paste(chosenSeason(),chosenYear())) %>%
      arrange(County) %>%
      select(County,Headcount,Rank)
    datatable(a,rownames = FALSE,options = list(pageLength = 10))
  })
  
  
  output$Map <- renderLeaflet({
    a <- all_data %>%
      filter(Term == paste(chosenSeason(),chosenYear())) %>%
      arrange(order)
    
    # max = round_any(max(a$Headcount), 100, f = ceiling)
    # quint <- (max-0)/5
    # bins <- c(0, quint, 2*quint,3*quint,4*quint,max)
    
    bins = quantile(a$Headcount,probs = seq(0, 1, 0.20))
    
    pal <- colorBin("Blues", domain = a$Headcount, bins = bins)
    labels <- sprintf("<strong>%s</strong><br/>Headcount: %g<br/>Rank: %g ", 
                      mapCo$NAME,a$Headcount,a$Rank) %>% lapply(htmltools::HTML)    
    leaflet() %>%
      setView(-79.8, 35.5, 6.5) %>%
      addPolygons(data = mapCo, fillColor = ~pal(a$Headcount), weight = 2, opacity = 1,
                  color = "white", dashArray = "3", fillOpacity = 0.7,group = "Counties",
                  highlight = highlightOptions(weight = 5, color = "#666", dashArray = "",
                                               fillOpacity = 0.7,bringToFront = TRUE),
                  label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                                           padding = "3px 8px"),
                                                              textsize = "15px", direction = "auto")) %>%
      addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL, position = "bottomright")
  })
  
  
  
  
}
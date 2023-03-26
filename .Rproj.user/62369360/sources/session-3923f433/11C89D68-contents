options(warn=-1)

# Packages <- c("plyr","dplyr","foreign","shiny", "DT", "leaflet", "datasets", "devtools", "maptools", "rgdal", "rmapshaper", "rgeos","tidyr")
# lapply(Packages, library, character.only = TRUE) # Load above packages
library(tidyverse)
library(plotly)

data <- read.csv("data.csv")
data$total <- data$tuition + data$fees

breakout <- read.csv("breakout.csv")
breakout$category <- factor(breakout$category, levels = rev(unique(breakout$category)))

function(input, output) {
  # UI REACTIVE INPUTS
  chosenYear <- reactive({    # Year input
    input$year
  })
  chosenEducationLevel <- reactive({    # Year input
    input$education_level
  })
  chosenFeeType <- reactive({    # Year input
    input$fee_type
  })
  
  # TEXTS
  output$table1_title <- renderText({
    paste0("<h4><center><b>", chosenEducationLevel(), " Tuition, Fees Academic Year ", chosenYear())
  })
  
  # HELPER FUNCTIONS
  line_chart <- function(state_level) {
    fee_type = ifelse(chosenFeeType() == "Total", "Tuition & Fees", chosenFeeType())
    {data %>%
        filter(education_level == chosenEducationLevel(), in_out_state == state_level) %>% 
        mutate(label = paste0("In <b>", year, "</b> the charge type <b>", fee_type, 
                              "</b> for <b>", chosenEducationLevel(), "</b> students is a total of ", 
                              paste0("<b>$", formatC(as.numeric(.[[tolower(chosenFeeType())]]), format="f", digits=0, big.mark=","), "</b>"))) %>% 
        ggplot(aes(x=year, y=.[[tolower(chosenFeeType())]], group=1)) +
        ggtitle(paste0(chosenEducationLevel(), " ", state_level, " ", fee_type)) +
        geom_line()+
        geom_point(aes(text=label)) +
        expand_limits(y=0) +
        scale_y_continuous(labels=scales::dollar_format(scale = .001, suffix = "K")) +
        theme_bw() +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title = element_text(hjust = 0.5)
        ) 
    } %>% 
      ggplotly(tooltip = "label")  %>%
      
      config(displayModeBar = F) %>% 
      layout(hoverlabel=list(bgcolor="white"))
  }
  
  bar_chart <- function(education){
    breakout %>%
      filter(education_level == education, year == chosenYear()) %>% 
      ggplot(aes(x=fees, y=category)) +
      ggtitle(paste0(education, " Fees, Academic Year ", chosenYear())) +
      geom_bar(stat = "identity", fill = "#5C86A7")+
      geom_text(aes(label = paste0("$", fees)), hjust = -0.1, nudge_x = 0.5) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(expand = c(.01, .01), labels=scales::dollar_format()) +
      scale_fill_identity(guide = "none") +
      theme_minimal() +
      theme(
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin = margin(15, 30, 15, 15)
      ) 
  }
  
  # OUTPUTS
  output$table1 <- renderDT({
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(colspan = 3, "In-State", style = "text-align:center; border-right: solid 1px; border-bottom: 0;"),
          th(colspan = 3, "Out-of-State", style = "text-align:center; border-bottom: 0;") 
        ),
        tr(
          mapply(th, 
                 rep(c("Tuition", "Fees", "Total"), 2), 
                 style = sprintf("border-right: solid %dpx; border-bottom: 0;", 
                                 c(0,0,1,0,0,0)), 
                 SIMPLIFY = FALSE)
        )
      )
    ))
    
    table1 <- data %>%
      filter(year == chosenYear(), education_level == chosenEducationLevel()) %>%
      mutate(across(c("tuition", "fees", "total"), ~ paste0("$", formatC(as.numeric(.x), format="f", digits=0, big.mark=",")))) %>% 
      pivot_wider(names_from = "in_out_state", 
                  values_from = c("tuition", "fees", "total")) %>%
      select(3, 5, 7, 4, 6, 8)
    
    datatable(table1, 
              container = sketch, 
              rownames = FALSE, 
              callback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
              options = list(dom = 't', 
                             # headerCallback = JS("$('table.dataTable.no-footer').css('border-bottom', 'none');"),
                             ordering=F, 
                             columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
      formatStyle(columns = names(table1), backgroundColor = "#FFFFFF") %>% 
      formatStyle(c(3), `border-right` = "solid 1px")
  })
  
  output$in_state <- renderPlotly(
    line_chart("In-State")
  )
  
  output$out_state <- renderPlotly(
    line_chart("Out-State")
  )
  
  output$undergraduate <- renderPlot(
    bar_chart("Undergraduate")
  )
  
  output$graduate <- renderPlot(
    bar_chart("Graduate")
  )
  
  
}
options(warn = -1)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)


# Define UI for dataset viewer application

data <- read.csv("data.csv")
years <- unique(data$year)
education_levels <- unique(data$education_level)


dashboardPage(
  dashboardHeader(title = "Fang Shi Demo"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '))),
    tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
    fluidRow(
      box(selectInput("year",
        label = "Select Year",
        choices = years, selected = years[1]
      ), width = 4),
      box(selectInput("education_level",
        label = "Select Education Level",
        choices = education_levels, selected = education_levels[1]
      ), width = 4),
      box(selectInput("fee_type",
                      label = "Select FeeType",
                      choices = c("Total", "Fees", "Tuition"), selected = "Total"
      ), width = 4)
    ),
    htmlOutput("table1_title"),
    fluidRow(
      DTOutput("table1")
    ),
    fluidRow(
      box(
        plotlyOutput("in_state")  
      ),
      box(
        plotlyOutput("out_state")  
      )
    ),
    fluidRow(
      box(
        plotOutput("undergraduate")  
      ),
      box(
        plotOutput("graduate")  
      )
    )
  )
)

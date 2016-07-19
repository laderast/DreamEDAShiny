
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
  navbarPage(
    
    # Application title
    "Viral Data Explorer",
    
    tabPanel("Summary",
             plotOutput("boxPlot"),
             verbatimTextOutput("dataSummary")
    ),
    
    # Sidebar with a slider input for number of bins
    tabPanel("Gene Explorer",
             selectInput("geneToFilter",label = "Select Gene To Filter By",
                         choices = genes,selected=genes[1]),
             plotOutput("genePlot")
    ),
    
    tabPanel("Pathway Explorer",
             selectInput("pathwayToFilter", label="Select Pathway to Display", 
                         choices = names(pathways), selected=names(pathways)[1]),
             plotOutput("pathwayPlot")
  )
)
)
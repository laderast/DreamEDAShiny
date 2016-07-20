
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$dataSummary <- renderPrint({summary(viralData)})
  
  
  output$boxPlot <- renderPlot({
    ggplot(viralData, aes(x=STUDYID, y=value)) + geom_boxplot() 
  })

  
  output$genePlot <- renderPlot({
    #get gene name from select box
    geneSelect <- input$geneToFilter
    
    #subset the data
    subsetData <- viralData[geneSymbol == geneSelect]
    
    #plot the data
    ggplot(subsetData, aes(x=TIMEHOURS, y=value, group=interaction(SUBJECTID, FEATUREID), 
                           color=interaction(SUBJECTID, FEATUREID))) + 
      geom_path() + facet_grid(.~STUDYID) + 
      #remove the color legend
      guides(colour=FALSE)
  })
  
  
  output$pathwayPlot <- renderPlot({
    #get pathway from select box
    pway <- input$pathwayToFilter
    #grab the pathway set associated with pathway
    pathwaySet <- pathways[[pway]]
    
    #subset the average profiles
    test <- averageProfiles[geneSymbol %in% pathwaySet]
    
    #plot the average profiles with error bars
    ggplot(test, aes(TIMEHOURS, meanExpr, group = geneSymbol, colour=geneSymbol)) + 
        geom_path() + facet_wrap(c("STUDYID")) + 
        geom_errorbar(aes(ymin=meanExpr-sdExpr, ymax=meanExpr+sdExpr))

  })
  
})

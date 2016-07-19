
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$genePlot <- renderPlot({
    
    geneSelect <- input$geneToFilter
    
    ggplot(viralData[geneSymbol == geneSelect], aes(x=TIMEHOURS, y=value, 
                                                    group=SUBJECTID, color=SUBJECTID)) + 
      geom_path() + facet_wrap(c("STUDYID")) + guides(colour=FALSE)
    
  })
  
  output$dataSummary <- renderPrint({summary(viralData)})
  
  output$boxPlot <- renderPlot({
    ggplot(viralData, aes(x=STUDYID, y=value)) + geom_boxplot() 
    
  })
  
  output$pathwayPlot <- renderPlot({
    
    
    if(is.null(input$pathwayToFilter)){
      pathwaySet <- pathways[[1]]
      pway <- names(pathways)[1]
    }else{
    print(input$pathwayToFilter)
    pway <- input$pathwayToFilter
    pathwaySet <- pathways[[pway]]
    }
    
    
    print(pway)
    print(pathwaySet)
    
    test <- averageProfiles[geneSymbol %in% pathwaySet]
    
#     ggplot(outData, 
#            aes(x=TIMEHOURS, y=meanExpr, 
#            group=geneSymbol, colour=geneSymbol)) +
#       geom_path()  +  geom_errorbar(aes(ymin=meanExpr - sdExpr, ymax=meanExpr + sdExpr)) + 
#       facet_wrap(c("STUDYID")) + ggtitle(pway) 
    
    ggplot(test, aes(TIMEHOURS, meanExpr, group = geneSymbol, colour=geneSymbol)) + geom_path() + facet_wrap(c("STUDYID")) + geom_errorbar(aes(ymin=meanExpr-sdExpr, ymax=meanExpr+sdExpr))

  })
  
})

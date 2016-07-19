library(data.table)
library(ggplot2)
library(dplyr)

load("/home/laderast/DreamEDAShiny/data/twoStudies.RData")

setkey(viralData, TIMEHOURS)

genes <- sort(unique(as.character(viralData$geneSymbol)),decreasing = FALSE)
studies <- unique(as.character(viralData$STUDYID))

#averageProfiles <- viralData[,.(meanExpr=mean(value),sdExpr=sd(value)),by=list(TIMEHOURS, STUDYID, geneSymbol)]

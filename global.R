library(data.table)
library(ggplot2)
library(dplyr)

<<<<<<< HEAD
load("data/twoStudies.RData")
=======
load("/home/laderast/DreamEDAShiny/data/twoStudies.RData")
>>>>>>> 9e43a5dc95ee6a6e3bedfda62ab9400bb9089b66

setkey(viralData, TIMEHOURS)

genes <- sort(unique(as.character(viralData$geneSymbol)),decreasing = FALSE)
studies <- unique(as.character(viralData$STUDYID))

#averageProfiles <- viralData[,.(meanExpr=mean(value),sdExpr=sd(value)),by=list(TIMEHOURS, STUDYID, geneSymbol)]

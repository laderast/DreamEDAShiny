library(data.table)
library(dplyr)

library(hgu133plus2.db)
x = unlist(as.list(hgu133plus2SYMBOL))
geneTable <- data.table(probeID = names(x), geneSymbol = x)
setkey(geneTable, probeID)

expression <- fread("data/ViralChallenge_training_EXPRESSION_RMA.tsv",sep = "\t",header = T)
clinicalDF <- read.delim("data/ViralChallenge_training_CLINICAL.tsv")

earlyResponse <- clinicalDF %>% filter(TIMEHOURS <= 36)

setkey(expression,FEATUREID)

expression2 <- as.data.frame(expression)

earlyData <- expression2[,colnames(expression2) %in% c("FEATUREID",as.character(earlyResponse$CEL))]

write.table(earlyData, "data/ViralChallenge_ExpressionRMA_early_timepoints.tsv", quote=F, sep="\t", row.names = FALSE)
rm(expression2)

write.table(earlyResponse, "data/ViralChallenge_CLINICAL_early_timepoints.tsv", quote=F, sep="\t", row.names = FALSE)

library(data.table)

studies <- as.character(unique(earlyResponse$STUDYID))

runOut <- lapply(studies, function(x){
  dataName <- paste0(x, "-ViralChallenge_Expression_early_timepoints.tsv")
  annotationName <- paste0(x, "-ViralChallenge_clinical_annotation.tsv")
  
  earlyStudy <- earlyResponse[earlyResponse$STUDYID == x,]
  dataSubset <- earlyData[,colnames(earlyData) %in% c("FEATUREID", as.character(earlyStudy$CEL))]
  
  outDir <- paste0("data/",x, "/")
  
  if(!dir.exists(outDir)){dir.create(outDir)}
  
  write.table(earlyStudy, paste0(outDir,annotationName), row.names = FALSE, sep="\t", quote=F)
  write.table(dataSubset, paste0(outDir, dataName), row.names=FALSE, sep="\t", quote=F)
})



library(data.table)

clinicalData <- read.delim("/home/laderast/DreamEDAShiny/data/Rhinovirus Duke/Rhinovirus Duke-ViralChallenge_clinical_annotation.tsv", header=TRUE)
expression <- fread("/home/laderast/DreamEDAShiny/data/Rhinovirus Duke/Rhinovirus Duke-ViralChallenge_Expression_early_timepoints.tsv")
setkey(expression, FEATUREID)

shamAnnotation <- clinicalData[!is.na(clinicalData$SHAM),]
shamCols <- colnames(expression)[colnames(expression) %in% c("FEATUREID",as.character(shamAnnotation$CEL))]
rhinoCols <- colnames(expression)[!colnames(expression) %in% shamAnnotation$CEL]
rhinoAnnotation <- data.table(clinicalData[is.na(clinicalData$SHAM),])

shamData <- expression[,shamCols,with=FALSE]


shamMelt <- melt(shamData,id.vars = c("FEATUREID"))
setkey(shamMelt, variable)

shamAnnotation <- data.table(shamAnnotation)
setkey(shamAnnotation, CEL)

#merge on sham annotation
shamMelt2 <- shamMelt[shamAnnotation, .(FEATUREID, value, AGE, GENDER, TIMEHOURS, STUDYID, SUBJECTID,SHEDDING_SC1, SYMPTOMATIC_SC2)]
setkey(shamMelt2, FEATUREID)

shamMelt2 <- shamMelt2[geneTable]
shamMelt2 <- shamMelt2[!is.na(geneSymbol)]



rhinoData <- expression[,c("FEATUREID",as.character(rhinoAnnotation$CEL)),with=FALSE]
rhinoMelt <- melt(rhinoData, id.vars=c("FEATUREID"))
setkey(rhinoMelt, variable)

setkey(rhinoAnnotation, CEL)

rhinoMelt2 <- rhinoMelt[rhinoAnnotation, .(FEATUREID, value, AGE, GENDER, TIMEHOURS, STUDYID, SUBJECTID, SHEDDING_SC1, SYMPTOMATIC_SC2)]




setkey(rhinoMelt2, FEATUREID)
rhinoMelt2 <- rhinoMelt2[geneTable]
rhinoMelt2 <- rhinoMelt2[!is.na(geneSymbol)]


#RFC2 <- rhinoMelt2[geneSymbol == "ABCA5"]
#RFC2mock <- shamMelt2[geneSymbol == "ABCA5"]

library(ggplot2)

ggplot(rhinoMelt2[geneSymbol == "CAMP"], aes(x=TIMEHOURS, y=value, group=SUBJECTID, colour=SUBJECTID)) + geom_line() + facet_wrap(c("SYMPTOMATIC_SC2"))
ggplot(shamMelt2[geneSymbol == "CAMP"], aes(x=TIMEHOURS, y=value, group=SUBJECTID, colour=SUBJECTID)) + geom_line() + facet_wrap(c("SYMPTOMATIC_SC2"))


rhinoMelt2[,mean(value), by="geneSymbol,TIMEHOURS"]


rhinoVirus <- expression[,rhinoCols, with=FALSE]


##calcluate CVs

cvRhino <- rhinoMelt2[!is.na(value),sd(value)/mean(value),by=geneSymbol]
setkey(cvRhino, geneSymbol)

cvSham <- shamMelt2[!is.na(value),sd(value)/mean(value),by=geneSymbol]
setkey(cvSham, geneSymbol)

cvs <- cvSham[cvRhino]
cvs[(cvRhino / cvSham) > 1.3]

setkey(cvs, geneSymbol)
setkey(rhinoMelt2, geneSymbol)

colnames(cvs) <- c("geneSymbol", "cvSham", "cvRhino")

out <- rhinoMelt2[cvs[abs(cvRhino / cvSham) > 1.4], mean(value), by="geneSymbol,TIMEHOURS"]

ggplot(out, aes(x=factor(TIMEHOURS), y=factor(as.character(geneSymbol)), fill=V1)) + geom_raster()


DEEH1N1annot <- fread("data/DEE3 H1N1/DEE3 H1N1-ViralChallenge_clinical_annotation.tsv")
setkey(DEEH1N1annot, CEL)

DEEH1N1data <- fread("data/DEE3 H1N1/DEE3 H1N1-ViralChallenge_Expression_early_timepoints.tsv")

DEEH1N1melt <- melt(DEEH1N1data, id.vars = "FEATUREID")
setkey(DEEH1N1melt, variable)

DEEH1N1melt <- DEEH1N1melt[DEEH1N1annot, .(FEATUREID, value, AGE, GENDER, TIMEHOURS, STUDYID, SUBJECTID, SHEDDING_SC1, SYMPTOMATIC_SC2)]
setkey(DEEH1N1melt, FEATUREID)

DEEH1N1melt <- DEEH1N1melt[geneTable]
DEEH1N1melt <- DEEH1N1melt[!is.na(geneSymbol)]

geneSyms <- unique(as.character(out$geneSymbol))


H1N1small <- DEEH1N1melt[geneSymbol %in% geneSyms]
H1N1small <- na.omit(H1N1small)
shamSmall <- shamMelt2[geneSymbol %in% geneSyms]
shamSmall$STUDYID <- "SHAM"

rhinoSmall <- rhinoMelt2[geneSymbol %in% geneSyms]

#save(H1N1small, shamSmall, rhinoSmall, file="data/twoStudies.RData")

viralList <- list(shamSmall, rhinoSmall, H1N1small)
viralData <- do.call(rbind, viralList)
viralData <- na.omit(viralData)



pathwaysFrame <- fread("/home/laderast/DreamEDAShiny/data/result.csv")

pathways <- lapply(pathwaysFrame$`Pathway name`, function(x){
  line <- pathwaysFrame[`Pathway name` == x]
  out <- strsplit(line$`Submitted entities found`, ";")[[1]]
  out
})

names(pathways) <- pathwaysFrame$`Pathway name`

averageProfiles <- viralData[,.(meanExpr=mean(value),sdExpr=sd(value)),by=list(TIMEHOURS, STUDYID, geneSymbol)]

save(viralData, pathways, averageProfiles, file="data/twoStudies.RData")

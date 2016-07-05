library(data.table)
expression <- fread("data/ViralChallenge_training_EXPRESSION_RMA.tsv",sep = "\t",header = T)
clinicalDF <- read.delim("data/ViralChallenge_training_CLINICAL.tsv")

earlyResponse <- clinicalDF %>% filter(TIMEHOURS <= 36)

expression2 <- as.data.frame(expression)
earlyData <- expression2[,colnames(expression2) %in% earlyCEL]
write.table(earlyData, "ViralChallenge_ExpressionRMA_early_timepoints.tsv", quote=F, sep="\t")
rm(expression2)

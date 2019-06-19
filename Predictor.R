load(file = "finalQuals.RData")
library(caret)

strErrorFix <- function(str, tokenArray, fixedArray, booleanFixed = FALSE){
  newArray <- sapply(tokenArray, grepl, str, ignore.case = TRUE, fixed = booleanFixed)
  index <- which(newArray == TRUE)[1]
  if(!is.na(index)){
    fixedArray[index]
  }else{
    str
  }
}

finalQuals[which(is.na(finalQuals$`Algebra exam 1`)),"Algebra exam 1"] <- 0
finalQuals[which(is.na(finalQuals$`Algebra exam 2`)),"Algebra exam 2"] <- 0
finalQuals[which(is.na(finalQuals$`Analisis exam 1`)),"Analisis exam 1"] <- 0
finalQuals[which(is.na(finalQuals$`IntroProg exam 1`)),"IntroProg exam 1"] <- 0
finalQuals$Name=NULL
finalQuals$Baja <- as.numeric(finalQuals$Baja)
library(dplyr)
finalQuals=finalQuals %>% mutate_if(is.numeric, as.character)
changing <- c("0.","1.","2.","3.","4.","5.","6.","7.","8.","9.","10.")
changed <- c("0","1","2","3","4","5","6","7","8","9","10")
finalQuals$`Algebra exam 1` <- sapply(finalQuals$`Algebra exam 1`, strErrorFix, changing, changed)
finalQuals$`Algebra exam 2` <- sapply(finalQuals$`Algebra exam 2`, strErrorFix, changing, changed)
finalQuals$`Analisis exam 1` <- sapply(finalQuals$`Analisis exam 1`, strErrorFix, changing, changed)
finalQuals$`IntroProg exam 1` <- sapply(finalQuals$`IntroProg exam 1`, strErrorFix, changing, changed)
finalQuals=finalQuals %>% mutate_if(is.character, as.numeric)
finalQuals$Baja <- as.factor(finalQuals$Baja)


index <- createDataPartition(finalQuals$Baja, p=0.75, list=FALSE)
trainSet <- finalQuals[ index,]
testSet <- finalQuals[-index,]

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

outcomeName<-'Baja'

#predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

Baja_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

predictors <- c("Analisis exam 1","IntroProg exam 1","Algebra exam 2","Algebra exam 1")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 4,
  repeats = 20)
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

library(gbm)
model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_gbm)  #AUC de 0.8
save(file = "model_gbm.rdata", model_gbm)

library(randomForest)
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
predictions<-predict.train(object=model_rf,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_rf)

library(nnet)
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
predictions<-predict.train(object=model_nnet,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_nnet) # AUC 0.8

library(glm2)
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
predictions<-predict.train(object=model_glm,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_glm)

library(snn)
model_snn<-train(trainSet[,predictors],trainSet[,outcomeName],method='snn')
predictions<-predict.train(object=model_snn,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_snn)

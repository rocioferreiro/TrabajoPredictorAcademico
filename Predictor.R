load("finalQuals.RData")
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


index <- createDataPartition(finalQuals$Baja, p=0.7, list=FALSE)
trainSet <- finalQuals[ index,]
testSet <- finalQuals[-index,]

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 15,
                      verbose = FALSE)

outcomeName<-'Baja'

predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]

Baja_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

Baja_Pred_Profile

predictors<-c("Analisis exam 1","IntroProg exam 1","Algebra exam 2","Algebra exam 1")

fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 20)
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))




model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
table(predictions)
library(gbm)
varImp(object=model_gbm)  #El mejor de todos 0.94

#save(file = "GBM/model_gbm.rdata", model_rf)
#save(file = "GBM/predictions.rdata", predictions)
#save(file = "GBM/testSet.rdata", testSet)


model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
predictions<-predict.train(object=model_rf,testSet[,predictors],type="raw")
table(predictions)
library(randomForest)
varImp(object=model_rf) # AUC 0.88
#save(file = "RF/model_rf.rdata", model_rf)
#save(file = "RF/predictions.rdata", predictions)
#save(file = "RF/testSet.rdata", testSet)


model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
predictions<-predict.train(object=model_nnet,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_nnet) # AUC 0.92
#save(file = "NNET/model_nnet.rdata", model_nnet)
#save(file = "NNET/predictions.rdata", predictions)
#save(file = "NNET/testSet.rdata", testSet)

model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')
predictions<-predict.train(object=model_glm,testSet[,predictors],type="raw")
table(predictions)
varImp(object=model_glm) #AUC 0.84
 #  save(file = "GLM/model_glm.rdata", model_glm)
 # save(file = "GLM/predictions.rdata", predictions)
 # save(file = "GLM/testSet.rdata", testSet)



plotROC <- function(pred){
  perf<- performance(pred,"tpr","fpr")
  plot(perf)
  AUC<-performance(pred,"auc")@y.values[[1]]
  grid()
  text(.6,.2,sprintf("AUC=%0.3f", AUC))
  abline(0,1,col="red", lty = 2)
}
# 
predaux<-prediction(as.numeric(predictions),testSet[,outcomeName])
# 
perf <- performance(predaux, "auc")
perf@y.values[[1]]
# 
plotROC(predaux)






predictor <- function(analisis, algebra1, algebra2, prog, model, output){
  if(model == "gbm"){
    load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/GBM/model_gbm.rdata")
    finalModel <- model_gbm
  } else if(model == "randomForest"){
    load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/RF/model_rf.rdata")
    finalModel <- model_rf
  } else if(model == "nnet"){
    load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/NNET/model_nnet.rdata")
    finalModel <- model_nnet
  } else {
    load(file = "~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/GLM/model_glm.rdata")
    finalModel <- model_glm
  }
  student <- data.frame(analisis,algebra1,algebra2,prog)
  names(student) <- c("Analisis exam 1", "IntroProg exam 1", "Algebra exam 1", "Algebra exam 2")
  predictors <- c("Analisis exam 1","IntroProg exam 1","Algebra exam 1","Algebra exam 2")
  output <- predict(finalModel,student)
}

save(file = "predictionFunction.rdata", predictor)


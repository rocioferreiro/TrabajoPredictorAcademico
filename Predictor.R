
load("~/Desktop/TpPredictorAcademico/TrabajoPredictorAcademico/finalQuals.RData")
library(caret)

finalQuals[which(is.na(finalQuals$`Algebra exam 1`)),"Algebra exam 1"] <- 2.01
finalQuals[which(is.na(finalQuals$`Algebra exam 2`)),"Algebra exam 2"] <- 2.01
finalQuals[which(is.na(finalQuals$`Analisis exam 1`)),"Analisis exam 1"] <- 2.01
finalQuals[which(is.na(finalQuals$`IntroProg exam 1`)),"IntroProg exam 1"] <- 2.01

finalQuals$Baja <- as.factor(finalQuals$Baja)

index <- createDataPartition(finalQuals$Baja, p=0.75, list=FALSE)
trainSet <- train_transformed[ index,]
testSet <- train_transformed[-index,]


model <- finalQuals[(1:70),]

rest <- finalQuals[(71:length(finalQuals)),]

hyp.out <- glm(Baja~`Algebra exam 1`+`Algebra exam 2`+`Analisis exam 1`+`IntroProg exam 1`,
               data=model, family="binomial")
summary(hyp.out)

coef(summary(hyp.out))

predsRest <- predict(hyp.out, rest, se.fit = TRUE)
# 
# boxplot(predsRest ~ NH11$hypev, col = c("green", "red"),
#         ylab = "Probabilidad",
#         xlab = "Tiene / No tiene hypertensión")


library(caret)

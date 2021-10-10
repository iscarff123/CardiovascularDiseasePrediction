

### Import Data
data <- read.csv("cardio3.csv")
data <- data[,-1]
data[,c(2,7:12)] <- lapply(data[,c(2,7:12)], factor)

library(caret)
library(corrplot)
library(e1071)


### Exploratory analysis


################## Continuous Variable ##################



### Scatter plots
contin <- data[,c(1,3,4,5,6)]
plot(contin, col = as.numeric(data$Disease)+1,
     cex = 0.5, pch = 16)

### Corr Plots
par(mfrow=c(1,2))
corrplot(cor(contin))
corrplot(cor(contin), method = "number")

### Histograms
par(mfrow = c(2,3))
for(i in 1:5){
  hist(contin[,i], xlab = colnames(contin)[i], main = colnames(contin)[i])
}


### Box Plots
par(mfrow=c(1,5))
for(i in 1:5){
  boxplot(contin[,i], xlab = colnames(contin)[i])
}

### Skewness

apply(contin, 2, skewness)
for(i in 1:5){
  print(abs(max(contin[i])/min(contin[i])))
}

### Possible transformations

apply(contin, 2, BoxCoxTrans)

### Quadratic transformation for Age
### Square-root transformation for Height
### Inverse square-root transformation for Weight
### Inverse transformation for Systolic
### No transformation for Daistolic

continTrans <- data.frame(contin$Age ^ 2, contin$Height ^ 0.5,
                          contin$Weight ^ -0.5, contin$Systolic ^ -1,
                          contin$Diastolic)


dev.off()
plot(continTrans, col = as.numeric(data$Disease)+1,
     cex = 0.5, pch = 16)

par(mfrow = c(2,3))
for(i in 1:5){
  hist(continTrans[,i], xlab = colnames(continTrans)[i])
}
apply(continTrans, 2, skewness)

corrplot(cor(continTrans))
corrplot(cor(continTrans), method = "number")

### Not much changed. 
### Try center and scaling

continPP <- preProcess(contin, method = c("scale","center"))
contin2 <- predict(continPP, contin)


dev.off()
plot(contin2, col = as.numeric(data$Disease)+1,
     cex = 0.5, pch = 16)


### Didn't change much.
### Try BoxCox, center and scaling

continPP <- preProcess(contin, method = c("scale","center","BoxCox"))
contin3 <- predict(continPP, contin)

plot(contin3, col = as.numeric(data$Disease)+1,
     cex = 0.5, pch = 16)

### Has increased the seperation of the target variable

par(mfrow = c(2,3))
for(i in 1:5){
  hist(contin3[,i], xlab = colnames(contin3)[i])
}
apply(contin3, 2, skewness)

### This looks good now

par(mfrow=c(1,2))
corrplot(cor(contin3))
corrplot(cor(contin3), method = "number")

### Correlations haven't really change.
### How much can one pair of correlated variable mess with classification?

### Use BoxCox, scaling, and centering



################## Categorical Variable ##################

cat <- data[,c(2,7,8,9,10,11,12)]

cat$Gender <- ifelse(cat$Gender == 0, "Female", "Male")
cat$Alcohol <- ifelse(cat$Alcohol == 0, "No", "Yes")
cat$Smoke <- ifelse(cat$Smoke == 0, "No", "Yes")
cat$Exercise <- ifelse(cat$Exercise == 0, "No", "Yes")
cat$Disease <- ifelse(cat$Disease == 0, "No", "Yes")

cat$Cholesterol <- ifelse(cat$Cholesterol == 0, "Normal",
                          ifelse(cat$Cholesterol == 1, "Above", "Well Above"))
cat$Glucose <- ifelse(cat$Glucose == 0, "Normal",
                          ifelse(cat$Glucose == 1, "Above", "Well Above"))



par(mfrow=c(2,4))
for(i in 1:7){
  barplot(table(cat[,i]), main = colnames(cat)[i])
}

### Predictors are unbalanced. Response is not.

### Check for near zero variance
for(i in 1:ncol(contin3)){
  print(nearZeroVar(contin3[,i]))
}
for(i in 1:ncol(cat)){
  print(nearZeroVar(cat[,i]))
}

### There are no near zero variance variables



################################# MODELING ######################

### Import Data
data <- read.csv("cardio3.csv")
data <- data[,-1]
data[,c(2,7:12)] <- lapply(data[,c(2,7:12)], factor)
#View(data)
#str(data)

library(caret)
library(corrplot)
library(e1071)


### Data Pre-Processing

### Remove Diastolic
data <- data[,-6]


par(mfrow=c(2,2))
hist(data$Weight, xlab = "Weight", main = "Weight Original")

### Apply transformations to Weight and Systolic
weightPP <- BoxCoxTrans(data$Weight)
WeightTrans <- predict(weightPP, data$Weight)


hist(WeightTrans, xlab = "Weight Tans", main = "Weight Transformed")

hist(data$Systolic, xlab = "Systolic", main = "Systolic Original")

systolicPP <- BoxCoxTrans(data$Systolic)
SystolicTrans <- predict(systolicPP, data$Systolic)

hist(SystolicTrans, xlab = "Systolic Trans", main = "Systolic Transformed")

### Put transformations in data
data$Weight <- WeightTrans
data$Systolic <- SystolicTrans

### Update column names
colnames(data)[c(4,5)] <- c("WeightTrans", "SystolicTrans")




par(mfrow=c(1,2))
corrplot(cor(data[,c(1,3:6)]))
corrplot(cor(data[,c(1,3:6)]), method = "number")



### Make a copy of the data.
### One will hold numeric factors
### One will hold string factors

data2 <- data



### Make catigorical variables strings
data2$Gender <- ifelse(data$Gender == 0, "Female", "Male")
data2$Alcohol <- ifelse(data$Alcohol == 0, "No", "Yes")
data2$Smoke <- ifelse(data$Smoke == 0, "No", "Yes")
data2$Exercise <- ifelse(data$Exercise == 0, "No", "Yes")
data2$Disease <- ifelse(data$Disease == 0, "No", "Yes")

data2$Cholesterol <- ifelse(data$Cholesterol == 0, "Normal",
                          ifelse(data$Cholesterol == 1, "Above", "Well Above"))
data2$Glucose <- ifelse(data$Glucose == 0, "Normal",
                      ifelse(data$Glucose == 1, "Above", "Well Above"))

data2[,c(2,6:11)] <- lapply(data2[,c(2,6:11)], factor)

### Create control function
set.seed(210)
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

### Create training and testing data
set.seed(210)
inTrain <- createDataPartition(data$Disease, p = 0.8)[[1]]
Train <- data[inTrain,]
Test <- data[-inTrain,]

Train2 <- data2[inTrain,]
Test2 <- data2[-inTrain,]



### Logistic Regression
set.seed(210)
logicTune  <- train(x = Train2[,c(1:10)], y = Train2$Disease,
                    method = "glm",
                    metric = "ROC",
                    trControl = ctrl)
logicTune

summary(logicTune)

logicPred <- predict(logicTune, Test2)
table(logicPred, Test2$Disease)

### Test error rate
mean(logicPred != Test2$Disease)

### Importance
varImp(logicTune)

### Test ROC
library(pROC)

logicROC <- roc(response = logicTune$pred$obs,
             predictor = logicTune$pred$Yes,
             levels = rev(levels(logicTune$pred$obs)))
plot(logicROC, legacy.axes = TRUE)
auc(logicROC)

### Save results

Test_Error <- c(mean(logicPred != Test2$Disease))
Test_AUC <- c(0.7885)
Models <- c("Logistic")


### LDA
set.seed(210)
ldaTune  <- train(form = Disease~., data = Train2,
                    method = "lda",
                    metric = "ROC",
                    trControl = ctrl)
ldaTune

ldaPred <- predict(ldaTune, Test2)
table(ldaPred, Test2$Disease)

### Test error rate
mean(ldaPred != Test2$Disease)

### Importance
varImp(ldaTune)

### Test ROC
library(pROC)

ldaROC <- roc(response = ldaTune$pred$obs,
                predictor = ldaTune$pred$Yes,
                levels = rev(levels(ldaTune$pred$obs)))
plot(ldaROC, legacy.axes = TRUE)
auc(ldaROC)

### Save results

Test_Error <- append(mean(ldaPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7882, Test_AUC)
Models <- append("LDA", Models)




### PLS DA
set.seed(210)
plsTune  <- train(form = Disease~., data = Train2,
                  method = "pls",
                  metric = "ROC",
                  tuneGrid = expand.grid(.ncomp = 1:11),
                  trControl = ctrl)
plsTune

plsPred <- predict(plsTune, Test2)
table(plsPred, Test2$Disease)

### Test error rate
mean(plsPred != Test2$Disease)

### Importance
varImp(plsTune)

### Test ROC
library(pROC)

plsROC <- roc(response = plsTune$pred$obs,
              predictor = plsTune$pred$Yes,
              levels = rev(levels(plsTune$pred$obs)))
plot(plsROC, legacy.axes = TRUE)
auc(plsROC)

### Save results

Test_Error <- append(mean(plsPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.6768, Test_AUC)
Models <- append("PLSDA", Models)



### Penalized Model
memory.limit(size = 20000)

set.seed(210)
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4),
                        .lambda = seq(.01, .2, length = 15))

penTune  <- train(form = Disease~., data = Train2,
                  method = "glmnet",
                  tuneGrid = glmnGrid,
                  metric = "ROC",
                  trControl = ctrl)
penTune

penPred <- predict(penTune, Test2)
table(penPred, Test2$Disease)

### Test error rate
mean(penPred != Test2$Disease)

### Importance
varImp(penTune)

### Test ROC
library(pROC)

penROC <- roc(response = penTune$pred$obs,
              predictor = penTune$pred$Yes,
              levels = rev(levels(penTune$pred$obs)))
plot(penROC, legacy.axes = TRUE)
auc(penROC)

### Save results

Test_Error <- append(mean(penPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7833, Test_AUC)
Models <- append("Pen", Models)



### Nearest Shrunken Centroids
nscGrid <- data.frame(.threshold = 0:25)
nscTune <- train(form = Disease~., data = Train2,
                  method = "pam",
                  tuneGrid = nscGrid,
                  metric = "ROC",
                  trControl = ctrl)
nscTune

nscPred <- predict(nscTune, Test2)
table(nscPred, Test2$Disease)

### Test error rate
mean(nscPred != Test2$Disease)

### Importance
varImp(nscTune)

### Test ROC
library(pROC)

nscROC <- roc(response = nscTune$pred$obs,
              predictor = nscTune$pred$Yes,
              levels = rev(levels(nscTune$pred$obs)))
plot(nscROC, legacy.axes = TRUE)
auc(nscROC)

### Save results

Test_Error <- append(mean(nscPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.6515, Test_AUC)
Models <- append("NSC", Models)



### QDA
set.seed(210)
qdaTune <- train(form = Disease~., data = Train2,
                 method = "qda", 
                 metric = "ROC",
                 trControl = ctrl)
qdaTune


qdaPred <- predict(qdaTune, Test2)
table(qdaPred, Test2$Disease)

### Test error rate
mean(qdaPred != Test2$Disease)

### Importance
varImp(qdaTune)

### Test ROC
library(pROC)

qdaROC <- roc(response = qdaTune$pred$obs,
              predictor = qdaTune$pred$Yes,
              levels = rev(levels(qdaTune$pred$obs)))
plot(qdaROC, legacy.axes = TRUE)
auc(qdaROC)

### Save results

Test_Error <- append(mean(qdaPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7561, Test_AUC)
Models <- append("QDA", Models)


### MDA
set.seed(210)
mdaTune <- train(form = Disease~., data = Train2,
                method = "mda", 
                metric = "ROC",
                tuneGrid = expand.grid(.subclasses = 1:10),
                trControl = ctrl)
mdaTune



mdaPred <- predict(mdaTune, Test2)
table(mdaPred, Test2$Disease)

### Test error rate
mean(mdaPred != Test2$Disease)

### Importance
varImp(mdaTune)

### Test ROC
library(pROC)

mdaROC <- roc(response = mdaTune$pred$obs,
              predictor = mdaTune$pred$Yes,
              levels = rev(levels(mdaTune$pred$obs)))
plot(mdaROC, legacy.axes = TRUE)
auc(mdaROC)

### Save results

Test_Error <- append(mean(mdaPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7855, Test_AUC)
Models <- append("MDA", Models)



### FDA
set.seed(210)
fdaTune <- train(form = Disease~., data = Train2,
                 method = "fda", 
                 metric = "ROC",
                 trControl = ctrl)
fdaTune

fdaPred <- predict(fdaTune, Test2)
table(fdaPred, Test2$Disease)

### Test error rate
mean(fdaPred != Test2$Disease)

### Importance
varImp(fdaTune)

### Test ROC
library(pROC)

fdaROC <- roc(response = fdaTune$pred$obs,
              predictor = fdaTune$pred$Yes,
              levels = rev(levels(fdaTune$pred$obs)))
plot(fdaROC, legacy.axes = TRUE)
auc(fdaROC)

### Save results

Test_Error <- append(mean(fdaPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7785, Test_AUC)
Models <- append("FDA", Models)


### Naive Bayes
set.seed(210)
nbTune <- train(x = Train2[,c(1:10)], y = Train2$Disease,
               method = "nb", 
               metric = "ROC",
               trControl = ctrl)
nbTune

nbPred <- predict(nbTune, Test2)
table(nbPred, Test2$Disease)

### Test error rate
mean(nbPred != Test2$Disease)

### Importance
varImp(nbTune)

### Test ROC
library(pROC)

nbROC <- roc(response = nbTune$pred$obs,
              predictor = nbTune$pred$Yes,
              levels = rev(levels(nbTune$pred$obs)))
plot(nbROC, legacy.axes = TRUE)
auc(nbROC)

### Save results

Test_Error <- append(mean(nbPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7818, Test_AUC)
Models <- append("NB", Models)


### KNN
set.seed(210)
knnTune <- train(form = Disease~., data = Train2,
                method = "knn",
                metric = "ROC",
                tuneGrid = data.frame(.k = c(1:100)),
                trControl = ctrl)
knnTune

knnPred <- predict(knnTune, Test2)
table(knnPred, Test2$Disease)

### Test error rate
mean(knnPred != Test2$Disease)

### Importance
varImp(knnTune)

### Test ROC
library(pROC)

knnROC <- roc(response = knnTune$pred$obs,
             predictor = knnTune$pred$Yes,
             levels = rev(levels(knnTune$pred$obs)))
plot(knnROC, legacy.axes = TRUE)
auc(knnROC)

### Save results

Test_Error <- append(mean(knnPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.6518, Test_AUC)
Models <- append("KNN", Models)


### Neural Network
set.seed(210)
nnetGrid <- expand.grid(.size = 1:10,
                        .decay = c(0, .01, .1, 0.5))
maxSize <- max(nnetGrid$.size)
numWts <-200
set.seed(210)
nnetTune <- train(form = Disease~., data = Train2,
                 method = "nnet",
                 metric = "ROC",
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 1000,
                 MaxNWts = numWts,
                 ## ctrl was defined in the previous chapter
                 trControl = ctrl)


nnetTune

nnetPred <- predict(nnetTune, Test2)
table(nnetPred, Test2$Disease)

### Test error rate
mean(nnetPred != Test2$Disease)

### Importance
varImp(nnetTune)

### Test ROC
library(pROC)

nnetROC <- roc(response = nnetTune$pred$obs,
              predictor = nnetTune$pred$Yes,
              levels = rev(levels(nnetTune$pred$obs)))
plot(nnetROC, legacy.axes = TRUE)
auc(nnetROC)

### Save results

Test_Error <- append(mean(nnetPred != Test2$Disease), Test_Error)
Test_AUC <- append(0.7636, Test_AUC)
Models <- append("NNet", Models)



############## Plots ###################



### Make plots comparing fit statistics

### Combine into one data frame
testResults <- data.frame(Test_Error,Test_AUC,Models)



### Test error rate, sorted
OrderErr <- testResults[order(testResults$Test_Error),]


plot(OrderErr$Test_Err, c(1:11), yaxt = "n",
     main = "Test Error Rate by Model",
     xlab = "Test Error Rate",
     ylab = "Model",
     pch = 16)
axis(2, at = c(1:11), labels = as.character(OrderErr$Models), las = 2,
     cex.axis = 0.8)
text(OrderErr$Test_Err[1:9], c(1:9),
     labels = as.character(round(OrderErr$Test_Err[1:9], 5)),
     adj = -0.2)
text(OrderErr$Test_Err[10:11], c(10:11),
     labels = as.character(round(OrderErr$Test_Err[10:11], 5)),
     adj = 1.2)




### AUC, sorted
OrderAUC <- testResults[order(testResults$Test_AUC),]


plot(OrderAUC$Test_AUC, c(1:11), yaxt = "n",
     main = "Test AUC by Model",
     xlab = "Area Under the Curve",
     ylab = "Model",
     pch = 16)
axis(2, at = c(1:11), labels = as.character(OrderAUC$Models), las = 2,
     cex.axis = 0.8)
text(OrderAUC$Test_AUC[1:5], c(1:5),
     labels = as.character(round(OrderAUC$Test_AUC[1:5], 5)),
     adj = -0.2)
text(OrderAUC$Test_AUC[6:11], c(6:11),
     labels = as.character(round(OrderAUC$Test_AUC[6:11], 5)),
     adj = 1.2)



### Make plot of ROC curves
plot(logicROC, legacy.axis = TRUE, lty = 5, lwd = 3.5, main = "ROC Curves by Model")
lines(ldaROC, col = "red", lty = 3, lwd = 4)
lines(plsROC, col = "blue", lty = 5, lwd = 3.5)
lines(penROC, col = "green", lty = 3, lwd = 4)
lines(nscROC, col = "magenta", lty = 5, lwd = 3.5)
lines(qdaROC, col = "purple", lty = 3, lwd = 4)
lines(mdaROC, col = "orange", lty = 5, lwd = 3.5)
lines(fdaROC, col = "forestgreen", lty = 5, lwd = 3.5)
lines(nbROC, col = "hotpink", lty = 3, lwd = 4)
lines(knnROC, col = "grey50", lty = 3, lwd = 4)
lines(nnetROC, col = "aquamarine", lty = 3, lwd = 4)

legend("topleft",
       legend = as.character(rev(testResults$Models)), 
       lty = c(5,3,5,3,5,3,5,5,3,3,3),
       lwd = c(3.5,4,3.5,4,3.5,4,3.5,3.5,4,4,4),
       col = c("black","red","blue","green","magenta","purple",
               "orange","forestgreen","hotpink","grey50","aquamarine"))



library(lattice)
resamp = resamples(list(Logistic = logicTune, LDA = ldaTune, PLSDA = plsTune,
                        Pen = penTune, NSC = nscTune, QDA = qdaTune,
                        MDA = mdaTune, FDA = fdaTune, NB = nbTune,
                        KNN = knnTune, NNet = nnetTune))

dotplot(resamp, metric = "ROC")

ModelDiff <- diff(resamp)
ModelDiff$statistics$ROC
























































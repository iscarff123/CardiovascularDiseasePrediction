

### Data Algorithms II
### Project Code


################## Data Cleaning #######################

### Import data
data <- read.csv("cardio.csv")
View(data)

### The purpose of this code is to clean the data and apply unit conversions from metri to imperial

### ID column can be removed
data <- data[,-1]

### Create better column names
varNames <- c("Age", "Gender", "Height", "Weight", "Systolic", "Diastolic",
              "Cholesterol", "Glucose", "Smoke", "Alcohol", "Exercise", "Disease")
colnames(data) <- varNames

### Age is in days, Height is in cm, Weight is in kg.
### Change these to years, inches, and pounds, respectively.

data$Age <- round(data$Age / 365.25, 2) ### This accounts for leap years
data$Height <- round(data$Height / 2.54, 2)
data$Weight <- round(data$Weight * 2.20462, 2)


### Gender: 1 = women, 2 = men.
### Cholesterol: 1 = normal, 2 = above normal, 3 = well above normal
### Glucose: 1 = normal, 2 = above normal, 3 = well above normal

### Subtract 1 from everyone of these variables to have a base level of zero
data$Gender <- data$Gender - 1
data$Cholesterol <- data$Cholesterol - 1
data$Glucose <- data$Glucose - 1


### Now convert all categorical variables to factor type
data[,c(2,7:12)] <- lapply(data[,c(2,7:12)], factor)


### Output new csv
write.csv(data, "cardio2.csv")


rm(list = ls())


data <- read.csv("cardio2.csv")
data <- data[,-1]
data[,c(2,7:12)] <- lapply(data[,c(2,7:12)], factor)

### Grab continuous variables
contin <- data[,c(1,3,4,5,6)]

### Look at boxplots

par(mfrow=c(1,5))
for (i in 1:5){
  boxplot(contin[,i], xlab = colnames(contin)[i])
}


### We can see that systolic and diastolic have massive outliers that make no physical sense. Why is this?
dev.off()
plot(contin$Systolic, contin$Diastolic)

### There are plenty of assumptions we could make to fix the data to our needs
### However, there are just too many that are needed for our analysis to still be valid
### Instead, we will focus on the BP range from hypotension to hypertension

data2 <- data[which(data$Systolic >= 50 & data$Systolic < 220),]
data2 <- data2[which(data2$Diastolic >= 20 & data2$Diastolic < 190),]

plot(data2$Systolic, data2$Diastolic)

### Remove cases where Diastolic is greater than systolic

data2 <- data2[-which(data2$Diastolic > data2$Systolic),]
plot(data2$Systolic, data2$Diastolic)


### Now lets look at height
quantile(data2$Height)

### Someone who suffers from Drwarfism has a height below 4ft 10in.
### For this study, we will look at subject's who are greater than 4ft
data2 <- data2[which(data2$Height >= 48),]

### Weight has some illogical values for adults. 
### Use values of weight above 80lb
data2 <- data2[which(data2$Weight >= 80),]


### Look at boxplots again
par(mfrow=c(1,5))
for (i in 1:5){
  boxplot(contin2[,i], xlab = colnames(contin2)[i])
}

### How much was removed?
1 - nrow(data2)/nrow(data)

### Only 2.05%. This is good.

### Output new csv
write.csv(data2, "cardio3.csv")


rm(list = ls())



##################### Data Preprocessing/ Exploration ###############


### Import Data
data <- read.csv("cardio3.csv")
data <- data[,-1]
data[,c(2,7:12)] <- lapply(data[,c(2,7:12)], factor)

library(caret)
library(corrplot)
library(e1071)


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

### Use BoxCox only


################## Categorical Variable ##################

##Gender
Bgender <- barplot((table(data$Gender)),
                   names.arg=c("Women","Men"),
                   col = c("Pink","lightblue"),
                   ylim = c(0, 50000),
                   main = "Gender")
text(x=Bgender, y= table(data$Gender),
     labels=as.character(table(data$Gender)),
     pos = 3,  
     col = "Black")


##Cholesterol
BCholesterol <- barplot((table(data$Cholesterol)),
                        names.arg=c("Normal", "Above normal", "Well above normal"),
                        col = c("lightgreen", "lightyellow", "red"),
                        ylim = c(0, 55000),
                        main = "Cholesterol")
text(x=BCholesterol, y= table(data$Cholesterol),
     labels=as.character(table(data$Cholesterol)),
     pos = 3,  
     col = "Black")


##Glucose
BGlucose <- barplot((table(data$Glucose)),
                    names.arg=c("Normal", "Above normal", "Well above normal"),
                    col = c("lightgreen", "lightyellow", "red"),
                    ylim = c(0, 65000),
                    main = "Glucose")
text(x=BGlucose, y= table(data$Glucose),
     labels=as.character(table(data$Glucose)),
     pos = 3,  
     col = "Black")


##Smoke
BSmoke <- barplot((table(data$Smoke)),
                  names.arg=c("Non-smoking", "smoking"),
                  col = c("lightgreen","gray"),
                  ylim = c(0, 70000),
                  main = "Smoking")
text(x=BSmoke, y= table(data$Smoke),
     labels=as.character(table(data$Smoke)),
     pos = 3,  
     col = "Black")


##Alcohol
BAlcohol <- barplot((table(data$Alcohol)),
                    names.arg=c("Non-drinking", "drinking"),
                    col = c("lightgreen","lightgray"),
                    ylim = c(0, 70000),
                    main = "Alcohol")
text(x=BAlcohol , y= table(data$Alcohol ),
     labels=as.character(table(data$Alcohol)),
     pos = 3,  
     col = "Black")


##Exercise
BExercise <- barplot((table(data$Exercise)),
                     names.arg=c("Non-exercising", "Exercising"),
                     col = c("lightgray","lightgreen"),
                     ylim = c(0, 60000),
                     main = "Exercising")
text(x=BExercise , y= table(data$Exercise),
     labels=as.character(table(data$Exercise)),
     pos = 3,  
     col = "Black")


##Disease
BDisease <- barplot((table(data$Disease)),
                    names.arg=c("NO", "YES"),
                    col = c("lightgray","red"),
                    ylim = c(0, 45000),
                    main = "If the person has cardiovascular disease")
text(x=BDisease , y= table(data$Disease),
     labels=as.character(table(data$Disease)),
     pos = 3,  
     col = "Black")


rm(list = ls())


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
plot(varImp(logicTune))

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
plot(varImp(ldaTune))

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
plot(varImp(plsTune))

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
plot(varImp(penTune))

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
plot(varImp(nscTune))

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
plot(varImp(qdaTune))

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
plot(varImp(mdaTune))

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
plot(varImp(fdaTune))

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
plot(varImp(nbTune))

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
plot(varImp(knnTune))

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
plot(varImp(nnetTune))

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
























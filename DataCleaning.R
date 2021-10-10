

### Had to first use the "Text to Columns" function in Excel

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



################## Continued #####################

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

### How much was removed?
1 - nrow(data2)/nrow(data)

### Only 1.91%

contin2 <- data2[,c(1,3,4,5,6)]


### Look at boxplots again
par(mfrow=c(1,5))
for (i in 1:5){
  boxplot(contin2[,i], xlab = colnames(contin2)[i])
}


### Now lets look at height
quantile(data2$Height)

### Someone who suffers from Drwarfism has a height below 4ft 10in.
### For this study, we will look at subject's who are greater than 4ft
data2 <- data2[which(data2$Height >= 48),]

contin2 <- data2[,c(1,3,4,5,6)]


### Look at boxplots again
par(mfrow=c(1,5))
for (i in 1:5){
  boxplot(contin2[,i], xlab = colnames(contin2)[i])
}


### Weight has some illogical values for adults. 
### Use values of weight above 80lb
data2 <- data2[which(data2$Weight >= 80),]

contin2 <- data2[,c(1,3,4,5,6)]


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












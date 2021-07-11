########################################
#Course: IST687
#Assignment: HW #9
#Name: Rohini Shrivastava
#Date: 9/2/20
#Notes: Rev 1.0
########################################
install.packages("kernlab")
install.packages("Metrics")
library(kernlab)
library(Metrics)

install.packages("ggplot2")
library(ggplot2)

install.packages("reshape2")
library(reshape2)
install.packages("e1071")
library(e1071)
install.packages("gridExtra")
library("gridExtra")

#STEP 1

df <- airquality
df <- na.omit(df)

#######################################
#Step 2

randIn <- sample(1:dim(df)[1])

cut1_3 <- floor(2*dim(df)[1]/3)

trainData<- df[randIn[1:cut1_3],]
testData <- df[randIn[(cut1_3+1):dim(df)[1]],]

trainData
testData
########################################
#STEP 3
ksvmOut <- ksvm(Ozone~., data=df, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
ksvmOut

ksvmTrain <- ksvm(Ozone~., data=trainData)#, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
ksvmTrain

ksvmTest <- ksvm(Ozone~., data=testData, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
ksvmTest

ksvmTest <- as.numeric(testData)

predict_test <- predict(ksvmTrain, testData)

predict_test <- as.numeric(predict_test)

root_square <- rmse(testData$Ozone, predict_test)
root_square

diff_ozone <- testData$Ozone - predict_test

df$Diff <- diff_ozone

scatter_plots <- ggplot(df, aes(x=temp, y=wind))+geom_point(aes(size=Diff, color=Diff))
scatter_plots

svmTrain <- svm(Ozone~., data=trainData)#, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
svmTrain

predict_test_svm <- predict(svmTrain, testData)
as.numeric(predict_test_svm)

diff_svm <- testData$Ozone - predict_test_svm

df$svm <- diff_svm

df
scatter_svm <- ggplot(df, aes(x=temp, y=wind))+geom_point(aes(size=svm, color=svm))
str(scatter_svm)

lm_test <- lm(ozone ~., data=df)
lm_test

predict_lm <- predict(lm_test, newdata = df)

diff_lm <- testData$Ozone - predict_lm

df$lm <- diff_lm

scatter_lm <- ggplot(df, aes(x=temp, y=wind))+geom_point(aes(size=lm, color=lm))
scatter_lm

grid.arrange(scatter_plots, scatter_svm, scatter_lm)

##########################################################
#STEP 4
newO <- c()
for (rows in 1:nrow(df)){
  if (df$Ozone[rows] == root_square || df$Ozone[rows] >root_square){
    newO[rows] <- 1
  } 
  else {newO[rows]<- 0}
}
newO
df$goodOzone <- newO
#########################################################
#STEP 5

randInn <- sample(1:dim(df)[1])

cut1_31 <- floor(2*dim(df)[1]/3)

trainData_good<- df[randInn[1:cut1_31],]
testData_good <- df[randInn[(cut1_31+1):dim(df)[1]],]

trainData_good
testData_good

ksvmTrain_good <- ksvm(goodOzone~., data=trainData_good)#, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
ksvmTrain_good

predict_test_good <- predict(ksvmTrain_good, testData_good)
predict_test_good<- as.numeric(predict_test_good)
testData_good$predict <-testData_good$goodOzone - predict_test_good

new1 <- c()
for (rows in 1:nrow(testData_good)){
  if (testData_good$goodOzone[rows] == predict_test_good[rows]){
    new1[rows] <- 1
  }
  else new1[rows]<- 0
}
new1

testData_good$predict
as.numeric(testData_good$Temp)



svmTrain_good <- svm(goodOzone~., data=trainData_good)#, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
svmTrain_good

predict_svm_good <- predict(svmTrain_good, testData_good)
as.numeric(predict_svm_good)
testData_good$predict_svm <-testData_good$goodOzone - predict_svm_good

lmTrain_good <- lm(goodOzone~., data=trainData_good)#, kernel="rbfdot", kpar="automatic", C=1, cross=3, prob.model=TRUE)
lmTrain_good

predict_lm_good <- predict(lmTrain_good, testData_good)
as.numeric(predict_lm_good)
testData_good$predict_lm <-testData_good$goodOzone - predict_lm_good


TESTING <- data.frame(testData_good$Temp, testData_good$Wind, testData_good$goodOzone, testData_good$predict_svm, testData_good$predict, testData_good$predict_lm)
colnames(TESTING) <-c("temp", "wind", "goodOzone", "predict_svm", "predict", "predict_lm")
str(TESTING)

scatter_ksvmgood <- ggplot(TESTING, aes(x=temp, y=wind))+geom_point(aes(size=goodOzone, color=predict))
scatter_ksvmgood

scatter_svmgood <- ggplot(TESTING, aes(x=temp, y=wind))+geom_point(aes(size=goodOzone, color=predict_svm))
scatter_svmgood

scatter_lmgood <- ggplot(TESTING, aes(x=temp, y=wind))+geom_point(aes(size=goodOzone, color=predict_lm))
scatter_lmgood

grid.arrange(scatter_ksvmgood, scatter_svmgood, scatter_lmgood, ncol=2)

###############################################################################
#STEP 6 
#From our models we can see that the predictions greately went up after we started including the goodOzone
#column into our data. Compared to step 3, step 5 was much more accurate. Our model looked best under the svm model.
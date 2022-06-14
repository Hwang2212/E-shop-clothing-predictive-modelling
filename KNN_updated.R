#Reading Data into a Dataframe
df <-read.csv("D:/1A Utar File/Y3S3/Predictive Modelling/Assignment/e-shop clothing 2008.csv", sep =";")
head(df)
#View(df)

#Taking out Year Column
df<- df[,-1]
df$session.ID <- NULL
df$day <- NULL
df$month <- NULL
#Install required packages
#install.packages("ISLR")
#####################################
## PERFORMING EDA
#####################################

library(ISLR)
library(skimr)
require(dplyr)
library(gtools)
library(tidyr)
library(class)
summary(df)
sum(is.na(df))
skim(df)
names(df)
dim(df)
df %>%
  dplyr::group_by(price.2) %>%
  skim()


df$page.2..clothing.model. <- as.factor(df$page.2..clothing.model.)
df$page.2..clothing.model. <- as.numeric(df$page.2..clothing.model.)
#Convert data to categorical data
col_factor = c("country","page.1..main.category.","colour","location","model.photography","price.2","page")
df[col_factor] = lapply(df[col_factor], factor)

df$price <- as.numeric(df$price)


#Stratified Sampling
set.seed(123)
train_size=0.75
df_p2.1= df[df$price.2 == "1",]
df_p2.2= df[df$price.2 == "2",]
price1_idx = sample(nrow(df_p2.1),size=round(train_size*nrow(df_p2.1)))
price2_idx = sample(nrow(df_p2.2),size=round(train_size*nrow(df_p2.2)))
df.train = rbind(df_p2.1[price1_idx,],df_p2.2[price2_idx,])
df.test = rbind(df_p2.1[-price1_idx,],df_p2.2[-price2_idx,])
summary(df.test)

#Getting SD for each column
sapply(df, function(x){sd(as.numeric)})

#Price Normalisation
normalise.vec <- function(column,ref.col) {
  return ((column - mean(ref.col)) / sd(ref.col))
}
df.train.knn = df.train
df.test.knn = df.test
df.train.knn$price = normalise.vec(df.train.knn$price, df.train$price)
df.test.knn$price = normalise.vec(df.test.knn$price,  df.test$price)

#Confusion Matrix
performance = function(xtab, description=""){
  cat(description,"\n")
  ACR = sum(diag(xtab))/sum(xtab)
  TPR = xtab[1,1]/sum(xtab[,1]); TNR = xtab[2,2]/sum(xtab[,2])
  PPV = xtab[1,1]/sum(xtab[1,]); NPV = xtab[2,2]/sum(xtab[2,])
  FPR = 1 - TNR                ; FNR = 1 - TPR
  # https://standardwisdom.com/softwarejournal/2011/12/confusion-matrix-another-single-value-metric-kappa-statistic/
  RandomAccuracy = (sum(xtab[,2])*sum(xtab[2,]) + 
                      sum(xtab[,1])*sum(xtab[1,]))/(sum(xtab)^2)
  Kappa = (ACR - RandomAccuracy)/(1 - RandomAccuracy)
  print(xtab)
  cat("\n      Accuracy :", ACR, "\n\n         Kappa :", Kappa, "\n")
  cat("\n   Sensitivity :", TPR,   "\n   Specificity :", TNR, "\n")
  cat("Pos Pred Value :", PPV,     "\nNeg Pred Value :", NPV, "\n")
  cat("           FPR :", FPR,     "\n           FNR :", FNR, "\n")
}

#Column setting for KNN Testing
col1 = c("page.1..main.category.", "page.2..clothing.model.","colour","model.photography","price","page") #Accuracy = 94.5587% at k = 131 at n=0.75
col2 = c("country","location","model.photography","page") 
col3 = c("country","page.1..main.category.", "page.2..clothing.model.","colour","location","model.photography","price","page") #Full model 
col4 = c("page.1..main.category.", "page.2..clothing.model.","colour","price") #Accuracy = 95.8% at k = 121 at n = 0.75
col5 = c("country","page.1..main.category.", "page.2..clothing.model.","colour","location","model.photography","price","price.2") #For Page Prediction

library(caret)
library(kknn)

#WKNN Modelling for price.2
df.price2.kknn = kknn(price.2~.-order, df.train.knn, df.test.knn, k=353)
yhat.price2.kknn = fitted(df.price2.kknn)
confusionMatrix(yhat.price2.kknn, df.test.knn$price.2)


#Cross Validation : 10-fold data
set.seed(123)
df_kfold = df[sample(nrow(df)),]
folds  = cut(seq(1, nrow(df)), breaks = 10, labels = F)
df_kfold_acc <- data.frame(fold = numeric(), accuracy = numeric())
#Train = 90%, Test = 10%
for(i in 1:10){
  testIndexes = which(folds == i, arr.ind=T)
  testData = df_kfold[testIndexes,]
  trainData = df_kfold[-testIndexes,]
  model = kknn 
  prediction = kknn(price.2 ~ .-order ,trainData, testData, k = 353)
  fit = fitted(prediction)
  cm = confusionMatrix(fit, testData[,9])
  print(cm)
  overall <- cm$overall
  overall.accuracy <- overall['Accuracy']
  df_kfold_acc[i,] <- c(i, overall.accuracy)
}
mean(df_kfold_acc$accuracy)




#Accuracy = 99.84771%

#Additional
#WKNN Modelling for Page
df.page.kknn = kknn(page~.-order, df.train.knn, df.test.knn, k=203)
yhat.page.kknn = fitted(df.page.kknn)
confusionMatrix(yhat.page.kknn, df.test.knn$page)
#Accuracy = 97.47%
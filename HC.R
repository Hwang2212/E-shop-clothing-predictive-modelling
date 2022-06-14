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
summary(df)
sum(is.na(df))
skim(df)

df %>%
  dplyr::group_by(price.2) %>%
  skim()

df$page.2..clothing.model. <- as.factor(df$page.2..clothing.model.)
df$page.2..clothing.model. <- as.numeric(df$page.2..clothing.model.)
#Convert data to categorical data
col_factor = c("country","page.1..main.category.","colour","location","model.photography","price.2","page")
df[col_factor] = lapply(df[col_factor], as.numeric)

df$price <- as.numeric(df$price)

df.label <- df$price.2
table(df.label)
n = (nrow(df))/2

normalise.vec <- function(column,ref.col) {
  return ((column - mean(ref.col)) / sd(ref.col))
}
#Data
col3 = c("country","page.1..main.category.","page.2..clothing.model.","colour","location","model.photography","price","page") #Full model 
df_data_full<- df[col3]
df_data_full$price <- normalise.vec(df_data_full$price,df$price)
#df_dist <- dist(df_data_full, method = "euclidean")
set.seed(1234)
df_train10k <- df[sample(1:nrow(df_data_full), 10000),]

set.seed(123)
df_data_d10k <- df_data_full[sample(1:nrow(df_data_full), 10000),]
#Distance 
df.dist10k= dist(df_data_d10k)
#hierarchical clustering Complete
hc.out_df.complete10k<- hclust(df.dist10k, method = "complete")
plot(hc.out_df.complete10k)
par(mfrow=c(1,1))
fit_clust = cutree(hc.out_df.complete10k, k = 2)
fit_clust
table (fit_clust)
rect.hclust(hc.out_df.complete10k, k = 2, border = "red")

library(cluster)
library(factoextra)
sil<-silhouette(fit_clust,df.dist10k)
fviz_silhouette(sil)

library(fpc)
HC_statistics <- cluster.stats(df.dist10k,fit_clust)

#Clustering Membership
agg.df <- aggregate(df_data_d10k, list(fit_clust), mean)
agg.df <- t(as.data.frame(agg.df))
agg.df <- agg.df[2:9,]

par(mfrow=c(1,2))
barplot(height=as.numeric(agg.df[,1]), main = paste("Predict Group 1 Score Mean"), ylab = "Score", ylim = c(0,15), names.arg=names(agg.df[,1]))
barplot(height=as.numeric(agg.df[,2]), main = paste("Predict Group 2 Score Mean"), ylab = "Score", ylim = c(0,15), names.arg=names(agg.df[,2]))

fit_clust<-as.factor(fit_clust)
str(fit_clust)

library(gmodels)
CrossTable(x=fit_clust,y=df_train10k$price.2)
library(caret)
confusionMatrix(fit_clust,as.factor(df_train10k$price.2))

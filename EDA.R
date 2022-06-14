#Reading Data into a Dataframe
df <-read.csv("D:/1A Utar File/Y3S3/Predictive Modelling/Assignment/e-shop clothing 2008.csv", sep =";")
head(df)
#View(df)

library(ISLR)
library(skimr)
require(dplyr)
library(gtools)
library(tidyr)
library(class)
library(tidyverse)

df$price <- as.numeric(df$price)
####
#Exploring Data
summary(df)
sapply(df, FUN = function(x) sum(is.na(df)))
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
df[1:14] = lapply(df[1:14], as.numeric)


#### Skewness and Kurtosis ####
library(moments)
s_country <- skewness(df$country)
s_category <- skewness(df$page.1..main.category)
s_colour <- skewness(df$colour)
s_location <- skewness(df$location)
s_model.photography <- skewness(df$model.photography)
s_price.2 <- skewness(df$price.2)
s_page <- skewness(df$page)

k_country <- kurtosis(df$country)
k_category <- kurtosis(df$page.1..main.category)
k_colour <- kurtosis(df$colour)
k_location <- kurtosis(df$location)
k_model.photography <- kurtosis(df$model.photography)
k_price.2 <- kurtosis(df$price.2)
k_page <- kurtosis(df$page)

par(mfrow = c(1, 3)) 
h_country <- hist(df$country)
h_category <- hist(df$page.1..main.category)
h_cloth <- hist(df$page.2..clothing.model.)
h_colour <- hist(df$colour)
h_location <- hist(df$location)
h_model.photography <- hist(df$model.photography)
h_price.2 <- hist(df$price.2)
h_page <- hist(df$page)

columns.skews <- c(s_country, s_category,s_colour,s_location,s_model.photography,s_price.2,s_page)
columns.kurts <- c(k_country, k_category,k_colour,k_location,k_model.photography,k_price.2,k_page)



#### Correlation ####

correlate_df_asc <- df %>%
  as.matrix %>% cor %>%
  `[<-`(lower.tri(., TRUE), NA) %>%
  as_tibble(rownames =  "Var 1") %>%
  pivot_longer(cols=-1, names_to="Var 2", values_to="Corr", values_drop_na=TRUE) %>%
  arrange(-abs(Corr))

correlate_df_asc <- correlate_df_asc[!(correlate_df_asc$`Var 1` == "month" |correlate_df_asc$`Var 2` == "month"  ),]

## Tail data ##
correlate_df_desc <- df %>%
  as.matrix %>% cor %>%
  `[<-`(lower.tri(., TRUE), NA) %>%
  as_tibble(rownames =  "Var 1") %>%
  pivot_longer(cols=-1, names_to="Var 2", values_to="Corr", values_drop_na=TRUE) %>%
  arrange(desc(-abs(Corr)))
  
correlate_df_desc <- correlate_df_desc[!(correlate_df_desc$`Var 1` == "session.ID " |correlate_df_desc$`Var 2` == "session.ID "  ),]
  
####
#Get maximum days 
month.days <- df %>%
  group_by(Month = as.numeric(month)) %>%
  summarise(date = max(day))
plot(month.days, cex = 3,pch = 13, col = "Blue", main = "Max Days for Each Month")
#Each month's data until their latest days except August at day 13


####
#Get Total Sales by Month
month.sales <- df %>%
  group_by(Month = as.numeric(month)) %>%
  summarise(Total_Sales = sum(price))

month.sales_bar = barplot(height=month.sales$Total_Sales, names=month.sales$Month, xlab = "Months", ylab = "Total Sales", main = "Total Sales by Month", col = "Red")

#April, July, May, June, August
# August was less because not complete


####
#Get Total Number of Sales each month
month.sales.no <- df %>%
  group_by(Month = as.numeric(month))%>%
  summarize(N =n())

month.sales.no_bar = barplot(height=month.sales.no$N, names=month.sales.no$Month, xlab = "Months", ylab = "Total Number of Sales", main = "Number of Sales by Month", col = "Blue")
#April has the highest number of sales made, August the least due to incomplete dataset


#####
#Get Total Sales by Clothing Category
cloth.sales <- df %>%
  group_by(Category = as.factor(page.1..main.category.)) %>%
  summarise(Total_Sales = sum(price))

#Replacing Numbers with Character of Category
cloth.sales$Category <- sub("1", "Trousers", cloth.sales$Category)
cloth.sales$Category <- sub("2", "Skirts", cloth.sales$Category)
cloth.sales$Category <- sub("3", "Blouses", cloth.sales$Category)
cloth.sales$Category <- sub("4", "On Sale", cloth.sales$Category)

cloth.sales_bar = barplot(height=cloth.sales$Total_Sales, names=cloth.sales$Category, xlab = "Category", ylab = "Total Sales in $", main = "Total Sales by Cloth Category", col = "Red")
cloth.sales_pie = pie(cloth.sales$Total_Sales, labels = cloth.sales$Category, main = "Total Sales by Clothing Category")


####
#Get Total Number of clothes sold by Clothing Category
cloth.no <- df %>%
  group_by(Category = as.factor(page.1..main.category.)) %>%
  summarise(N = n())

#Replacing Numbers with Character of Category
cloth.no$Category <- sub("1", "Trousers", cloth.sales$Category)
cloth.no$Category <- sub("2", "Skirts", cloth.sales$Category)
cloth.no$Category <- sub("3", "Blouses", cloth.sales$Category)
cloth.no$Category <- sub("4", "On Sale", cloth.sales$Category)


piepercent<- round(100*cloth.no$N/sum(cloth.no$N), 1) 

cloth.no_bar = barplot(height=cloth.no$N, names=cloth.no$Category, xlab = "Category", ylab = "Total Clothes Sold", main = "Total Clothes Sold by Cloth Category", col = "Blue")
cloth.no_pie = pie(cloth.no$N, labels = cloth.no$Category, col = rainbow(length(cloth.no$N)) , main = "Total Clothes Sold by Clothing Category")

#Trousers has the highest number of pieces sold at 49742, Skirts has the least pieces sold at 38408, Onsale the second at 38747, and blouses at 38577


####
#Get Average Price of each category
cloth.avgp <- df %>%
  group_by(Category = as.factor(page.1..main.category.)) %>%
  summarise(Average = sum(price)/ n())

#Replacing Numbers with Character of Category
cloth.avgp$Category <- sub("1", "Trousers", cloth.sales$Category)
cloth.avgp$Category <- sub("2", "Skirts", cloth.sales$Category)
cloth.avgp$Category <- sub("3", "Blouses", cloth.sales$Category)
cloth.avgp$Category <- sub("4", "On Sale", cloth.sales$Category)


cloth.avgp_bar = barplot(height=cloth.avgp$Average, names=cloth.no$Category, xlab = "Category", ylab = "Average Price", main = "Average Price by Cloth Category", col = "Green")

#Skirts sells at a higher price, followed by trousers, Blouses, and clothes on sale



####
#Get Total Number of clothes sold by Page
cloth.page <- df %>%
  group_by(Page=as.factor(page)) %>%
  summarise(N = n())

cloth.page_bar = barplot(height=cloth.page$N, names=cloth.page$Page, xlab = "Page", ylab = "Total Clothes Sold", main = "Total Clothes Sold by Page", col = "Purple")

.#Page 1 has the highest Number of Clothes sold, followed by 2,3,4,5




#####
#Get Total Number of clothes sold by Page
cloth.page.price <- df %>%
  group_by(Page=as.factor(page)) %>%
  summarise(Sales=sum(price))

cloth.page.price_bar = barplot(height=cloth.page.price$Sales, names=cloth.page.price$Page, xlab = "Page", ylab = "Total Sales", main = "Total Sales Made by Page", col = "Purple")

#Page 1 has the highest Sales at $4 Million, followed by 2,3,4,5


###
#Get Number of Clothes based on Category and Page
cloth.sale.page <- df %>%
  group_by(Category = as.factor(page.1..main.category.), Page = as.factor(page)) %>%
  summarise(N= n())

cloth.sale.page1 <- cloth.sale.page %>% xtabs(formula = N ~ Category + Page) %>% as.data.frame()
cloth.sale.page1 <- arrange(cloth.sale.page1, Category) #Rearrange Data

#Replacing Numbers with Character of Category
cloth.sale.page1$Category<- sub("1", "Trousers", cloth.sale.page1$Category)
cloth.sale.page1$Category <- sub("2", "Skirts", cloth.sale.page1$Category)
cloth.sale.page1$Category <- sub("3", "Blouses", cloth.sale.page1$Category)
cloth.sale.page1$Category<- sub("4", "On Sale", cloth.sale.page1$Category)

#Creating Vectors for Stacked Bar Chart
Page <-  c("1","2","3","4","5")
colours <-  c("red","blue","green","orange","brown")
Category <- c("Trousers", "Skirts", "Blouses", "On Sale")

#Extracting Value for Stacked Bar Chart
Values <- t(matrix(cloth.sale.page1$Freq, nrow = 4, ncol = 5, byrow = TRUE))
cloth.sale.page_bar = barplot(Values, names.arg = Category, xlab = "Category", ylab = "No of Clothes Sold", 
                              main = "No of Clothes Sold by Category and Page", col = colours)
legend("topright", Page, cex = 1.3, fill = colours) #Add Legends to Stacked Bar Chart

# Trousers has the highest no. of clothes sold, highest at page 1, the least sales on page 3 and no sales at page 4,5.
# Skirts are the second highest no. sold. Highest No. sold at page 2 but none afterwards
# Blouses has the third no of clothes sold, highest at page 3, but none at page 5.
# Clothes on Sale is the only item that was sold on all 5 page.



####
#Get Total Sales by Category and Page
cloth.sale.page.price <- df %>% xtabs(formula = price ~ page.1..main.category. + page) %>% as.data.frame()
cloth.sale.page.price <- arrange(cloth.sale.page.price, page.1..main.category.) #Rearrange Data

#Replacing Numbers with Character of Category
cloth.sale.page.price$page.1..main.category.<- sub("1", "Trousers", cloth.sale.page.price$page.1..main.category.)
cloth.sale.page.price$page.1..main.category. <- sub("2", "Skirts", cloth.sale.page.price$page.1..main.category.)
cloth.sale.page.price$page.1..main.category. <- sub("3", "Blouses", cloth.sale.page.price$page.1..main.category.)
cloth.sale.page.price$page.1..main.category.<- sub("4", "On Sale", cloth.sale.page.price$page.1..main.category.)

#Creating Vectors for Stacked Bar Chart
Page <-  c("1","2","3","4","5")
colours <-  c("red","blue","green","orange","brown")
Category <- c("Trousers", "Skirts", "Blouses", "On Sale")

#Extracting Value for Stacked Bar Chart
Values1 <- t(matrix(cloth.sale.page.price$Freq, nrow = 4, ncol = 5, byrow = TRUE))
cloth.sale.page.price_bar = barplot(Values1, names.arg = Category, xlab = "Category", ylab = "Total Sales in $", 
                              main = "Total Sales by Category and Page", col = colours)
legend("topright", Page, cex = 1.3, fill = colours) #Add Legends to Stacked Bar Chart


# Trousers has the highest Sales, highest at page 1, the least sales on page 3 and no sales at page 4,5.
# Skirts are the second highest sales. Highest No. sold at page 2 but none afterwards
# Blouses has the third highest sales, highest at page 3, but none at page 5.
# Clothes on Sale is the only item that was sold on all 5 page.


#####
# See which clothing category has more products that are sold at prices higher than the average price
cloth.price.2 <- df %>%
  group_by(Category = as.factor(page.1..main.category.), AvgP = as.factor(price.2)) %>%
  summarise(N= n())

cloth.price2 <- cloth.price.2 %>% xtabs(formula = N ~ Category+AvgP) %>% as.data.frame()
cloth.price2<- arrange(cloth.price2, Category)

#Replacing Numbers with Character of Category
cloth.price2$Category<- sub("1", "Trousers", cloth.price2$Category)
cloth.price2$Category <- sub("2", "Skirts", cloth.price2$Category)
cloth.price2$Category <- sub("3", "Blouses", cloth.price2$Category)
cloth.price2$Category<- sub("4", "On Sale", cloth.price2$Category)

#Creating Vectors for Stacked Bar Chart
Price.2 <-  c("Higher than Average Price", "Lower than Average Price")
colours2 <-  c("green","orange")
Category <- c("Trousers", "Skirts", "Blouses", "On Sale")


#Extracting Value for Stacked Bar Chart
Values2 <- t(matrix(cloth.price2$Freq, nrow = 4, ncol = 2, byrow = TRUE))
cloth.price2_bar = barplot(Values2, names.arg = Category, xlab = "Category", ylab = "No. of Clothes", 
                                    main = "No. of Clothes by Comparison of Average Price", col = colours2)
legend("topright", Price.2, cex = 1.3, fill = colours2) #Add Legends to Stacked Bar Chart

#Trousers was sold more at lower prices than average among the 4 categories
#Skirts was sold more at higher than average prices.
#No. of Blouses and Clothes on Sale sold are balanced at average prices
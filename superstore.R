superDF <- read.csv("Superstore.csv")

library(Hmisc)
library(tidyr)

#describe(superDF)
#summary(superDF)
colSums(is.na(superDF))

#no NA. try figure out new variable.
superDF$Value <- (1+(superDF$Discount))*100

superDF$Revenue.Unit <- (superDF$Sales/superDF$Quantity)

superDF$Profit.Unit <- (superDF$Profit/superDF$Quantity)

superDF$Unit.Cost <- superDF$Revenue.Unit - superDF$Profit.Unit

superDF$Market.Value <- (superDF$Unit.Cost/100)*superDF$Value

superDF <- separate(superDF,'Order.Date',into=c("Year","Month","Day"),sep="-")

#write.csv(superDF, file = "D:/superstorenew.csv")
#============================================================================================================#

#superDF <- read.csv(file="D:/superstorenew.csv")

#============================================================================================================#

superDF$Year <- as.factor(superDF$Year)

library(ggplot2)
library(scales)
library(dplyr)

technology <- superDF %>% filter(Category== "Technology") %>% group_by(Year, Sub.Category, Region, Sales)

ggplot(technology, aes(x=Year , y=Sales , fill=Sub.Category))+
  geom_boxplot()+
  facet_wrap(~Region , scales = "free_y" )+
  expand_limits(y=0)

 


#####################################################################
# Market and Segment generating most profit
plot1<-ggplot(Sales.Summary.Total,aes(x=Sales.Summary.Total$Market,y=Sales.Summary.Total$TotalProfit,fill=Sales.Summary.Total$Segment))
plot1+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit")+ggtitle("Total Profit")

# Market and Segment having most profit margin
plot2<-ggplot(Sales.Summary.Total,aes(x=Sales.Summary.Total$Market,y=Sales.Summary.Total$ProfitPercent,fill=Sales.Summary.Total$Segment))
plot2+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Profit %age")+ggtitle("Profit percent")

# Market and Segment having most profit margin
plot3<-ggplot(Sales.Summary.Total,aes(x=Sales.Summary.Total$Market,y=Sales.Summary.Total$CVMonthlyProfitPercent,fill=Sales.Summary.Total$Segment))
plot3<-plot3+geom_bar(stat="identity",position="dodge")+xlab("Market")+ylab("Coeff. of variance of monthly profit")
plot3+ggtitle("Coeff. of variance in monthly profit Vs. Market Segment")


#============================================================================================================#


#============================================================================================================#
#superDF <- read.csv("Superstore.csv")
library(dplyr)

#check and remove outlier
boxplot(superDF$Unit.Cost)
outliers1 <- boxplot(superDF$Unit.Cost, plot=FALSE)$out
costoutlier <- superDF[which(superDF$Unit.Cost %in% outliers1),]
superDF_NoOutlier <- superDF[-which(superDF$Unit.Cost %in% outliers1),]
boxplot(superDF_NoOutlier$Unit.Cost)

#vairables used for clustering
#superDF_select_Furniture=superDF_NoOutlier %>% filter(Category=="Furniture") %>% select("Quantity","UNIT.COST")
#superDF_select_Office_Supplies=superDF_NoOutlier %>% filter(Category=="Office Supplies") %>% select("Quantity","UNIT.COST")
#superDF_select_Technology=superDF_NoOutlier %>% filter(Category=="Technology") %>% select("Quantity","UNIT.COST")

superDF_select_Technology = superDF %>% filter(Category=="Technology") %>% select("Quantity", "Unit.Cost") %>% sample_frac(0.01)


###replace variable superDF_select_XXX to run clustering plot

set.seed(10)
wcss = vector() #initialise empty vector
for (i in 1:10) wcss[i] = sum(kmeans(superDF_select_Technology, i)$withinss) #i=number of clusters
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS') #number of datapoints in the cluster i

set.seed(30)
kmeans = kmeans(x = superDF_select_Technology, centers = 3)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(superDF_select_Technology,
         y_kmeans,
         lines = TRUE,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Technology customers'),
         xlab = 'Unit.Cost',
         ylab = 'Quantity')


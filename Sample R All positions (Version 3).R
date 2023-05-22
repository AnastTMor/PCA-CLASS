# Install pre-requisite packages#
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")

#Load required libraries#
library(stats)
library(dplyr)
library(ggfortify)

#Data input, clean and processing (must be unlabelled)#

library("readxl")
read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\readxl2.xlsx")
data<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\readxl2.xlsx")
data$'3P%'<-data$'3P%'*100
data$'eFG%'<-data$'eFG%'*100
data$'FG%'<-data$'FG%'*100
data$'FT%'<-data$'FT%'*100
data$'2P%'<-data$'2P%'*100
View(data)

data_averages<-data%>%
group_by(Player)%>%
summarise(
`eFG%`=round(mean(`eFG%`),2),
`3P%` = round(mean(`3P%`),2),
`3PA` = round(mean(`3PA`),2),
`3P` = round(mean(`3P`),2),
'AST' = round(mean(AST),2),
'BLK' = round(mean(BLK),2),
`FG%` = round(mean(`FG%`),2),
'FGA' = round(mean(FGA),2),
'FG' = round(mean(FG),2),
`FT%` = round(mean(`FT%`),2),
'FTA' = round(mean(FTA),2),
'FT' = round(mean(FT),2),
'G' = round(mean(G),2),
'MP' = round(mean(MP),2),
'PTS' = round(mean(PTS),2),
'TRB' = round(mean(TRB),2),
'STL' = round(mean(STL),2),
'TOV' = round(mean(TOV),2)
  )
View(data_averages)

#The dataset must have only non NA values#
Complete_data<-data_averages[complete.cases(data_averages),]
View(Complete_data)

newdata<-subset(Complete_data,
select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19))
newdata$Player_ID<-1:nrow(newdata)
View(newdata)

no_name_data<- round(newdata[,-1])
View(no_name_data)

#Creation of Frequency Tables#

freq_table<-table(rounded_data)
View(freq_table)

pts_freq_table<-table(select(rounded_data,c(16,4)))
View(pts_freq_table)

#Main aim of PCA is to transform or combine linearly correlated variables into a 
new set of variables called Principle Components#

#We must check if the variabes of the dataset are highly correlated with one another.#
#Whether they are linearly dependent or not#
#Correlation matrix: If average correlation is>0.3 or <0.3 there is evidence that the
#variables are highly correlated amongst one another and are hence eligible for Principle# 
#Component Analysis (PCA)#

#Check PCA Eligiblity#
#Build Correlation Matrix#
#In order to work, the dataset must be composed of complete number, not NA#



newdata_cor<-newdata[,-c(1,2,3,9,12,19)]
View(newdata_cor)
correl<-cor(newdata_cor)
correl_rounded<-round(correl,2)
View(correl_rounded)

#Average Correlation amongst variables of dataset#
#Neglects and NA value in the correl#

mean(correl, na.rm=TRUE)

#or if we have neglected earlier the NA values#

mean(correl)

#Creation of Graphical Display of the Correlation Matrix#
library(corrplot)
corrplot(correl_rounded, type="upper",order="hclust",
tl.col="black",tl.srt=45)

#Principle Component Analysis (PCA)#

PCA<-princomp(newdata_cor)

#Evaluation of PCA analysis#

#Two ways to evaluate PCA:#
#1. Check wether PCs capture the essence of the original varables#
#2. Check wether PCs are independent or not.#

#PC Loadings#

loadings_15<-PCA$loadings
PC1_weights15<-loadings_15[,1]
PC2_weights15<-loadings_15[,2]

View(PC1_weights15)
View(PC2_weights15)

fviz_pca_var(PCA, col.var = "contrib", col.ind = "cos2")

#Principle Components#
Correlation Matrix on Principle Components to see wether they are independent#

PC=PCA$scores
CORREL=cor(PC)
View(CORREL)
---------------------------------------------------------------------------------
#To create communication channel between to datasets using Player_ID in order to#
#show the corresponding names of each player on the graph#

merged_data<- merge(rounded_data,newdata, by="Player_ID")

#Clustering#
k<-3

set.seed(123)
kmeans_model <- kmeans(PC, centers = k)


newdata_rounded$cluster<-kmeans_model$cluster
merged_data$cluster <- kmeans_model$cluster

#Scatter point graph#

ggplot(subset(merged_data,PC[,1]>0 & PC[,2]>0),
aes(x=PC[,1], y=PC[,2], color=factor(cluster))) +
geom_point() +
scale_color_discrete(name="Cluster") +
ggtitle("K-Means Clustering")

#Scatter with names#

ggplot(merged_data, aes(x=PC[,1], y=PC[,2], color=factor(cluster))) +
geom_point() +
geom_text(aes(label=Player),hjust=0, vjust=0) +
scale_color_discrete(name="Cluster") +
ggtitle("K-Means Clustering")


wcss <- sapply(1:10, function(k) {
kmeans(PC, k, nstart = 50)$tot.withinss
})


plot(1:10, wcss,
type = "b", pch = 19, frame = FALSE,
xlab = "Number of Clusters",
ylab = "Within Sum of Squares")



-----------------------------------------------------------------------------
#Extracting info of Clusters#

cluster1<-subset(merged_data,cluster==1)
cluster2<-subset(merged_data,cluster==2)
cluster3<-subset(merged_data,cluster==3)

player_name <- "Nikola Jokić"
player_cluster <- subset(merged_data, Player == player_name)$cluster
player_cluster 


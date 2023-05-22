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
data_15<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2015-2016.xlsx")
data_16<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2016-2017.xlsx")
data_17<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2017-2018.xlsx")
data_18<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2018-2019.xlsx")
data_20<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2020-2021.xlsx")
data_21<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2021-2022.xlsx")
data_22<-read_excel("C:\\Users\\user\\Desktop\\DATA ANALYSIS\\2022-2023.xlsx")

#Data cleaning#
data_15$'3P%'<-data_15$'3P%'*100
data_15$'eFG%'<-data_15$'eFG%'*100
data_15$'FG%'<-data_15$'FG%'*100
data_15$'FT%'<-data_15$'FT%'*100
data_15$'2P%'<-data_15$'2P%'*100

data_16$'3P%'<-data_16$'3P%'*100
data_16$'eFG%'<-data_16$'eFG%'*100
data_16$'FG%'<-data_16$'FG%'*100
data_16$'FT%'<-data_16$'FT%'*100
data_16$'2P%'<-data_16$'2P%'*100

data_17$'3P%'<-data_17$'3P%'*100
data_17$'eFG%'<-data_17$'eFG%'*100
data_17$'FG%'<-data_17$'FG%'*100
data_17$'FT%'<-data_17$'FT%'*100
data_17$'2P%'<-data_17$'2P%'*100

data_18$'3P%'<-data_18$'3P%'*100
data_18$'eFG%'<-data_18$'eFG%'*100
data_18$'FG%'<-data_18$'FG%'*100
data_18$'FT%'<-data_18$'FT%'*100
data_18$'2P%'<-data_18$'2P%'*100

data_20$'3P%'<-data_20$'3P%'*100
data_20$'eFG%'<-data_20$'eFG%'*100
data_20$'FG%'<-data_20$'FG%'*100
data_20$'FT%'<-data_20$'FT%'*100
data_20$'2P%'<-data_20$'2P%'*100

data_21$'3P%'<-data_21$'3P%'*100
data_21$'eFG%'<-data_21$'eFG%'*100
data_21$'FG%'<-data_21$'FG%'*100
data_21$'FT%'<-data_21$'FT%'*100
data_21$'2P%'<-data_21$'2P%'*100

data_22$'3P%'<-data_22$'3P%'*100
data_22$'eFG%'<-data_22$'eFG%'*100
data_22$'FG%'<-data_22$'FG%'*100
data_22$'FT%'<-data_22$'FT%'*100
data_22$'2P%'<-data_22$'2P%'*100

data_averages15<-data_15%>%
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
View(data_averages15)

Complete_data15<-data_averages15[complete.cases(data_averages15),]
View(Complete_data15)

newdata15<-subset(Complete_data15,
select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19))
newdata15$Player_ID<-1:nrow(newdata15)
View(newdata15)

#Extract in Excel#

library(openxlsx)

# Set the file path and name
file_path <- ("C:\Users\user\Desktop\DATA ANALYSIS\2015-2016.xlsx")

# Write the dataframe to the Excel file
write.xlsx(newdata15, file_path, sheetName = "Sheet1", row.names = FALSE)

no_name_data15<- round(newdata15[,-1])
View(no_name_data15)

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

newdata_cor15<-newdata15[,-c(1,14,19)]
View(newdata_cor15)
correl_15<-cor(newdata_cor15)
correl_rounded15<-round(correl_15,2)
View(correl_rounded15)

mean(correl_15, na.rm=TRUE)

library(corrplot)
corrplot(correl_rounded15, type="upper",order="hclust",
tl.col="black",tl.srt=45)

PCA_15<-princomp(newdata_cor15)

loadings_15<-PCA_15$loadings
PC1_weights15<-loadings_15[,1]
PC2_weights15<-loadings_15[,2]

View(PC1_weights15)
View(PC2_weights15)

fviz_pca_var(PCA_15, col.var = "contrib", col.ind = "cos2")
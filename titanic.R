setwd("C:/Users/NungLianSume/Desktop/Kaggle Datasets/Titanic")

#Lirary
library(caret)
library(ggplot2)
library(GGally)

#Get data
rawtraining <- read.csv("train.csv",stringsAsFactors = FALSE)
fianltest <- read.csv('test.csv',stringsAsFactors = FALSE)

#Check Data
head(rawtraining)
head(finaltest)

str(rawtraining)

nrow(rawtraining)
nrow(finaltest)

#Make the target as factor
rawtraining$Survived <- as.factor(rawtraining$Survived)

#Check missing
checkingMissing <- function(data){
  rownm <- names(data)
  varclass <- sapply(data,class)
  isNULL <- sapply(data,is.null)
  isNA <- sapply(lapply(data,is.na),sum)
  result <- rbind(varclass,isNULL,isNA)
  return(t(result))
}

checkingMissing(rawtraining)


#Check Embark
table(rawtraining$Embarked)
rawtraining$Embarked[rawtraining$Embarked == ""] = "S"

selectedVar <- names(rawtraining)[!names(rawtraining) %in% c("PassengerId","Name","Ticket","Cabin")]


#Data Partition
training <- rawtraining[,selectedVar]

inTrain <- createDataPartition(training$Survived,p=0.8,list = FALSE,times = 1)
training <- training[inTrain,]
testing <- training[-inTrain,]

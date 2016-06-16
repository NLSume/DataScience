setwd("C:/Users/Nung Lian Sume/Desktop/AAA Challenge")

training <- read.csv('SAStraining.csv',stringsAsFactors = FALSE)
training$readmitted <- as.factor(training$readmitted)
testing <- read.csv('SAStest.csv',stringsAsFactors = FALSE)

variables <- names(training)
varexcludes <- c("patientID","admissionDate","diag_1_desc","diag_2_desc","diag_3_desc","discharge_disposition_id",'admission_source_id')

trainingFilter <- training[, !variables %in% varexcludes]



#Convert from Character to Factor
trainingFilter[sapply(trainingFilter,is.character)] <- lapply(trainingFilter[sapply(trainingFilter,is.character)],as.factor)

#Get library
library(ggplot2)
# Load in the packages to create a fancified version of your tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Amelia)
library(vcd)

missmap(trainingFilter,main = "Missing Map",
                      col=c("yellow",'black'),
                      legend = FALSE)

#Identified missing variables
missingVars <- c("weight","payer_code","medical_specialt")

#Check the number of missing / na
round(sum(is.na(trainingFilter$weight))/nrow(trainingFilter),2)
round(sum(is.na(trainingFilter$payer_code))/nrow(trainingFilter),2)
round(sum(is.na(trainingFilter$medical_specialt))/nrow(trainingFilter),2)



#Drop vairables with high missing values
trainingFilter <- trainingFilter[,!names(trainingFilter) %in% missingVars]


#Visualization
table(trainingFilter$readmitted)
round(prop.table(table(trainingFilter$readmitted)),2)

ggplot(data=trainingFilter,
       aes(x=readmitted,fill=readmitted)) +
  geom_bar(stat='count',color="black",width = 0.7) + 
  guides(fill=FALSE) + 
  ylab("Total readmitted") + 
  xlab("Readmitted (1=Yes,0=No)") +
  ggtitle("Proportion of Readmitted vs Non-readmitted")




mosaicplot(trainingFilter$age ~ trainingFilter$readmitted,
           main = "Readmitted by Age Group",shade = FALSE,
           color = TRUE,
           xlab = "Age Groups",
           ylab = "Readmitted")


mosaicplot(trainingFilter$metformin ~ trainingFilter$readmitted,
           main = "Readmitted by Age Group",shade = FALSE,
           color = TRUE,
           xlab = "Age Groups",
           ylab = "Readmitted")

ggplot(data = trainingFilter,
       aes(x=metformin,fill=readmitted)) +
  geom_bar(stat='count')

ggplot(data = trainingFilter,
       aes(x=repaglinide,fill=readmitted)) +
  geom_bar(stat='count')

ggplot(data = trainingFilter,
       aes(x=nateglinide,fill=readmitted)) +
  geom_bar(stat='count')


ggplot(data = trainingFilter,
       aes(x=acarbose,fill=readmitted)) +
  geom_bar(stat='count')

ggplot(data = trainingFilter,
       aes(x=medical_specialty,fill=readmitted)) +
  geom_bar(stat='count')


# Time to plot your fancified tree
fancyRpartPlot(my_tree_two)
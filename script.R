setwd("D:/Nung Lian Sume/R Version 3.3.0/AAA Challenge")
library(ggplot2)
library(ggthemes)
library(GGally)

#library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Amelia)
library(vcd)

#Getting Data
rawtraining <- read.csv('SAStraining.csv',stringsAsFactors = FALSE)
finaltest <- read.csv('SAStest.csv',stringsAsFactors = FALSE)

#Initial Data Preparation
rawtraining$readmitted <- as.factor(rawtraining$readmitted)
variables <- names(rawtraining)
initialExcludes <- c("patientID","admissionDate","diag_1_desc","diag_2_desc","diag_3_desc","discharge_disposition_id","admission_source_id")

train <- rawtraining[,!variables %in% initialExcludes]

#Convert Character to Factor
train[sapply(train,is.character)] <- lapply(train[sapply(train,is.character)],as.factor)
str(train)

#Check missing values
missmap(train,main = "Missing value mapping",
        col = c('yellow','black'),
        legend = FALSE)

#Identified high missing value
missingVars <- c("weight","payer_code","medical_specialty")


#Check the number of missing value
round(sum(is.na(train$weight))/nrow(train),2)
round(sum(is.na(train$payer_code))/nrow(train),2)
round(sum(is.na(train$medical_specialty))/nrow(train),2)


#Exclude high missing value variales
train <- train[,!names(train) %in% missingVars]
str(train)


#Check Zerow/Low Variance variables
nzv <- nearZeroVar(train,saveMetrics = TRUE)
zerovariance <- rownames(nzv[nzv$zeroVar == TRUE,])

#Exclude zerovariance
train <- train[,!names(train) %in% zerovariance]
str(train)
names(train)


####################################################################
#--------------- Visualization ------------------------------------#

#Creates varialbes groups
profiles <- names(train)[1:3]
hospitals <- names(train)[4:11]

#Check proportion
table(train$readmitted)
percent <- round(prop.table(table(train$readmitted)),2)

sprintf("The proportion of target variable is:- Yes: %.1f percent and No: %.1f percent",percent[[1]]*100,percent[[2]]*100)


ggplot(data=train,aes(x=readmitted,fill=readmitted)) +
  geom_bar(stat='count',color='black',width = 0.7) +
  guides(fill = FALSE) +
  ylab("Total readmitted") +
  xlab("Readmitted (1=Yes,0=No)") +
  ggtitle("Readmitted vs. Non-readmitted")

mosaicplot(train$age ~ train$readmitted,
           main = "Readmitted by Age Groups",
           color = TRUE,
           shade = FALSE,
           xlab = "Age Groups",
           ylab = "Readmitted")

mosaicplot(~ gender + readmitted,
           data=train,
           main = "Readmitted by Gender",
           color = TRUE,
           shade = FALSE,
           xlab = "Gender",
           ylab = "Readmitted")

ggpairs(train[,c(profiles,'readmitted')])
ggpairs(train[,c(hospitals,'readmitted')])


ggplot(data=train,
       aes(x=readmitted,y=time_in_hospital)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("By Time in Hospital") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=num_lab_procedures)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No. of Lab Procedure") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=num_procedures)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Procedure") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=num_medications)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Medication") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=number_outpatient)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Outpatients") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=number_emergency)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Emergency") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=number_inpatient)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("Number of Inpatient") +
  theme_economist()

ggplot(data=train,
       aes(x=readmitted,y=number_diagnoses)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Diagnoses") +
  theme_economist()

#Factor variable Visualization
mosaicplot(~ max_glu_serum + readmitted,
           data=train,
           main = "max_glu_serum",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ A1Cresult + readmitted,
           data=train,
           main = "A1Cresult",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ metformin + readmitted,
           data=train,
           main = "metformin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ repaglinide + readmitted,
           data=train,
           main = "repaglinide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ nateglinide + readmitted,
           data=train,
           main = "nateglinide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ chlorpropamide + readmitted,
           data=train,
           main = "chlorpropamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glimepiride + readmitted,
           data=train,
           main = "glimepiride",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glipizide + readmitted,
           data=train,
           main = "glipizide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glyburide + readmitted,
           data=train,
           main = "glyburide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ tolbutamide + readmitted,
           data=train,
           main = "tolbutamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ pioglitazone + readmitted,
           data=train,
           main = "pioglitazone",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glyburide + readmitted,
           data=train,
           main = "glyburide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ tolbutamide + readmitted,
           data=train,
           main = "tolbutamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ pioglitazone + readmitted,
           data=train,
           main = "pioglitazone",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ rosiglitazone + readmitted,
           data=train,
           main = "rosiglitazone",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ acarbose + readmitted,
           data=train,
           main = "acarbose",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ miglitol + readmitted,
           data=train,
           main = "miglitol",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ tolazamide + readmitted,
           data=train,
           main = "tolazamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ insulin + readmitted,
           data=train,
           main = "insulin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glyburide.metformin + readmitted,
           data=train,
           main = "glyburide.metformin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ max_glu_serum + readmitted,
           data=train,
           main = "max_glu_serum",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glipizide.metformin + readmitted,
           data=train,
           main = "glipizide.metformin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ change + readmitted,
           data=train,
           main = "change",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ diabetesMed + readmitted,
           data=train,
           main = "diabetesMed",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ readmitted + readmitted,
           data=train,
           main = "readmitted",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")


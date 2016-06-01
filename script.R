setwd("D:/Nung Lian Sume/R Version 3.3.0/AAA Challenge")
#setwd("C:/Users/Nung Lian Sume/Desktop/AAA Challenge")

library(ggplot2)
library(ggthemes)
library(GGally)

#library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Amelia)
library(vcd)

library(caret)

###############################################################################
######---------------------- Getting Data  -----------------------#############
rawtraining <- read.csv('SAStraining.csv',stringsAsFactors = FALSE)
finaltest <- read.csv('SAStest.csv',stringsAsFactors = FALSE)

#Initial Data Preparation
rawtraining$readmitted <- as.factor(rawtraining$readmitted)
variables <- names(rawtraining)
initialExcludes <- c("patientID","admissionDate")

train <- rawtraining[,!variables %in% initialExcludes]


###############################################################################
######---------------- Handling Missing Values -------------------#############

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

#Check if the missing values has relationshp with readmittance
train$weightR[!is.na(train$weight)] <- 'No-Missing'
train$weightR[is.na(train$weight)] <- 'Missing'

train$payer_codeR[!is.na(train$payer_code)] <- 'No-Missing'
train$payer_codeR[is.na(train$payer_code)] <- 'Missing'

train$medical_specialtyR[!is.na(train$medical_specialty)] <- 'No-Missing'
train$medical_specialtyR[is.na(train$medical_specialty)] <- 'Missing'

#Weight
weightTable <- table(train$readmitted,train$weightR)
print(weightTable)
round(prop.table(weightTable,margin = 2),2)
xWeight <- chisq.test(train$weightR,train$readmitted)
print(xWeight)

#payer_code
payerTable <- table(train$readmitted,train$payer_codeR)
print(payerTable)
round(prop.table(payerTable,margin = 2),2)
xPayer <- chisq.test(train$payer_codeR,train$readmitted)
print(xPayer)

#medical_specialty
specialtyTable <- table(train$readmitted,train$medical_specialtyR)
print(specialtyTable)
round(prop.table(specialtyTable,margin = 2),2)
xSpecialty <- chisq.test(train$medical_specialtyR,train$readmitted)
print(xSpecialty)


train$weightR <- as.factor(train$weightR)
train$payer_codeR <- as.factor(train$payer_codeR)
train$medical_specialtyR <- as.factor(train$medical_specialtyR)

#Exclude high missing value variales
train <- train[,!names(train) %in% missingVars]
str(train)

#Check and replace blank cases
train$race <- ifelse(train$race == "?","NotKeyed",train$race)
train$admission_type_id <- ifelse(train$admission_type_id == "","NotKeyed",train$admission_type_id)
train$admission_source_id <- ifelse(train$admission_source_id == "","NotKeyed",train$admission_source_id)
train$discharge_disposition_id <- ifelse(train$discharge_disposition_id == "","NotKeyed",train$discharge_disposition_id)


################################################################################
#------ Cleaning Diag_1, Diag_2 and Diag_3 ------------------------------------#

diagnosis <- rawtraining[c('diag_1','diag_2','diag_3','readmitted')]

#Creating Dictionary for Coding
dia1 <- rawtraining[,c("diag_1","diag_1_desc")]
names(dia1) <- c('diag','description')
dia2 <- rawtraining[,c("diag_2","diag_2_desc")]
names(dia2) <- c('diag','description')
dia3 <- rawtraining[,c("diag_3","diag_3_desc")]
names(dia3) <- c('diag','description')

allDiag <- rbind(dia1,dia2,dia3)
allDiagUnique <- unique(allDiag)
allDiagUnique <- allDiagUnique[order(allDiagUnique$diag),]
print(head(allDiagUnique,10))

#Check cases with 250 Codes
diag250 <- allDiagUnique[grepl('250',allDiagUnique$diag),]
diagnosis250 <- diagnosis[diagnosis$diag_1 %in% diag250$diag,]
table(diagnosis250$diag_1,diagnosis250$readmitted)
round(prop.table(table(diagnosis250$diag_1,diagnosis250$readmitted),margin=1),2)

##############################################################################
#---------Shifting/Reorganizing the diag_1,diag_2 and diag_3-----------------#

#If the diag_1 is missing, replace with diag_2
diagnosis$diag_1 <- ifelse(diagnosis$diag_1 == '?',diagnosis$diag_2,diagnosis$diag_1)
#If the diag_2 is missing, replace with diag_3
diagnosis$diag_2 <- ifelse(diagnosis$diag_2 == '?',diagnosis$diag_3,diagnosis$diag_2)
#If diag_1 and diag_2 is the same, repalce diag_2 with diag 3
diagnosis$diag_2 <- ifelse(diagnosis$diag_1 == diagnosis$diag_2,diagnosis$diag_3,diagnosis$diag_2)
#If diag_2 and diag_3 is the same, repalce diag_3 with '?', if not with diag_3
diagnosis$diag_3 <- ifelse(diagnosis$diag_2 == diagnosis$diag_3,'?',diagnosis$diag_3)


diagnosisMissing <- diagnosis[diagnosis$diag_1 == '?' |
                                diagnosis$diag_2 == '?' |
                                diagnosis$diag_3 == '?',]
head(diagnosisMissing,15)




#Replace Missing Values

fix_missing <- function(x,na.value){
  x[x==na.value] <- NA
  x
}
diagnosis[1:3] <- lapply(diagnosis[1:3],fix_missing,'?')

#Merge diagnosis into the train dataset
train[,c('diag_1','diag_2','diag_3')] <- NULL
diagnosis[,'readmitted'] <- NULL
train <- cbind(train,diagnosis)

################################################################################
#------------------- Derive New Variables  -----------------------------------#

#No of Important Diagnosis
train$No_Diags <- apply(train[,1:3],1,function(x) sum(!is.na(x)))
round(prop.table(table(train$No_Diags,train$readmitted),margin=1),2)


#Hospice
hospicecode <- c("Hospice / home","Hospice / medical facility")
Hospice <- train[train$discharge_disposition_id %in% hospicecode,c('discharge_disposition_id','readmitted','No_Diags')]
#Check if patients who were sent to Hospice were not readmitted (possibily died)
round(prop.table(table(Hospice$discharge_disposition_id,Hospice$readmitted),margin = 1),2)
round(prop.table(table(Hospice$discharge_disposition_id,Hospice$No_Diags),margin = 1),2)

train$hospice <- ifelse(train$discharge_disposition_id %in% c("Hospice / home","Hospice / medical facility"), 1,0)

################################################################################
#------ #Check Zerow/Low Variance variables -----------------------------------#

nzv <- nearZeroVar(train,saveMetrics = TRUE)
print(nzv)
zerovariance <- rownames(nzv[nzv$zeroVar == TRUE,])
print(zerovariance)

#Exclude zerovariance
train <- train[,!names(train) %in% zerovariance]
str(train)
names(train)


################################################################################
#----------------------- Prepare trian data -----------------------------------#

#To exclude
diaDescription <- c('diag_1_desc','diag_2_desc','diag_3_desc')
train <- train[,!(names(train) %in% diaDescription)]


#Convert Character to Factor
train[sapply(train,is.character)] <- lapply(train[sapply(train,is.character)],as.factor)
str(train)

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



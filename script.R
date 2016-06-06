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
initialExcludes <- c("patientID")

trainData <- rawtraining[,!variables %in% initialExcludes]


###############################################################################
######---------------- Handling Missing Values -------------------#############

#Check missing values
missmap(trainData,main = "Missing value mapping",
        col = c('yellow','black'),
        legend = FALSE)

#Identified high missing value
missingVars <- c("weight","payer_code","medical_specialty")


#Check the number of missing value
round(sum(is.na(trainData$weight))/nrow(trainData),2)
round(sum(is.na(trainData$payer_code))/nrow(trainData),2)
round(sum(is.na(trainData$medical_specialty))/nrow(trainData),2)

#Check if the missing values has relationshp with readmittance
trainData$weightR[!is.na(trainData$weight)] <- 'No-Missing'
trainData$weightR[is.na(trainData$weight)] <- 'Missing'

trainData$payer_codeR[!is.na(trainData$payer_code)] <- 'No-Missing'
trainData$payer_codeR[is.na(trainData$payer_code)] <- 'Missing'

trainData$medical_specialtyR[!is.na(trainData$medical_specialty)] <- 'No-Missing'
trainData$medical_specialtyR[is.na(trainData$medical_specialty)] <- 'Missing'

#Weight
weightTable <- table(trainData$readmitted,trainData$weightR)
print(weightTable)
round(prop.table(weightTable,margin = 2),2)
xWeight <- chisq.test(trainData$weightR,trainData$readmitted)
print(xWeight)

#payer_code
payerTable <- table(trainData$readmitted,trainData$payer_codeR)
print(payerTable)
round(prop.table(payerTable,margin = 2),2)
xPayer <- chisq.test(trainData$payer_codeR,trainData$readmitted)
print(xPayer)

#medical_specialty
specialtyTable <- table(trainData$readmitted,trainData$medical_specialtyR)
print(specialtyTable)
round(prop.table(specialtyTable,margin = 2),2)
xSpecialty <- chisq.test(trainData$medical_specialtyR,trainData$readmitted)
print(xSpecialty)


trainData$weightR <- as.factor(trainData$weightR)
trainData$payer_codeR <- as.factor(trainData$payer_codeR)
trainData$medical_specialtyR <- as.factor(trainData$medical_specialtyR)

#Exclude high missing value variales
trainData <- trainData[,!names(trainData) %in% missingVars]
str(trainData)

#Check and replace blank cases
trainData$race <- ifelse(trainData$race == "?","NotKeyed",trainData$race)
trainData$admission_type_id <- ifelse(trainData$admission_type_id == "","NotKeyed",trainData$admission_type_id)
trainData$admission_source_id <- ifelse(trainData$admission_source_id == "","NotKeyed",trainData$admission_source_id)
trainData$discharge_disposition_id <- ifelse(trainData$discharge_disposition_id == "","NotKeyed",trainData$discharge_disposition_id)


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

#Merge diagnosis into the trainData dataset
trainData[,c('diag_1','diag_2','diag_3')] <- NULL
diagnosis[,'readmitted'] <- NULL
trainData <- cbind(trainData,diagnosis)

#Recoding Diag_1, Diag_2 and Diag_3
trainData$diag_1R <- ifelse(grepl('(V)|(E)',trainData$diag_1),'9999',trainData$diag_1)
trainData$diag_1R <- as.numeric(trainData$diag_1R)
trainData$diag_1R <- ifelse(trainData$diag_1R <= 139, "A", 
                          ifelse(trainData$diag_1R <= 239, "B", 
                          ifelse(trainData$diag_1R <= 279, "C", 
                          ifelse(trainData$diag_1R <= 289, "D", 
                          ifelse(trainData$diag_1R <= 319, "E", 
                          ifelse(trainData$diag_1R <= 359, "F", 
                          ifelse(trainData$diag_1R <= 389, "G", 
                          ifelse(trainData$diag_1R <= 459, "H", 
                          ifelse(trainData$diag_1R <= 519, "I", 
                          ifelse(trainData$diag_1R <= 579, "J", 
                          ifelse(trainData$diag_1R <= 629, "K", 
                          ifelse(trainData$diag_1R <= 679, "L", 
                          ifelse(trainData$diag_1R <= 709, "M", 
                          ifelse(trainData$diag_1R <= 739, "N", 
                          ifelse(trainData$diag_1R <= 759, "O", 
                          ifelse(trainData$diag_1R <= 779, "P", 
                          ifelse(trainData$diag_1R <= 799, "Q", 
                          ifelse(trainData$diag_1R <= 999, "R",
                          ifelse(trainData$diag_1R <= 9999,"EV",'NA')))))))))))))))))))

trainData$diag_2R <- ifelse(grepl('(V)|(E)',trainData$diag_2),'9999',trainData$diag_2)
trainData$diag_2R <- as.numeric(trainData$diag_2R)
trainData$diag_2R <- ifelse(trainData$diag_2R <= 139, "A", 
                          ifelse(trainData$diag_2R <= 239, "B", 
                          ifelse(trainData$diag_2R <= 279, "C", 
                          ifelse(trainData$diag_2R <= 289, "D", 
                          ifelse(trainData$diag_2R <= 319, "E", 
                          ifelse(trainData$diag_2R <= 359, "F", 
                          ifelse(trainData$diag_2R <= 389, "G", 
                          ifelse(trainData$diag_2R <= 459, "H", 
                          ifelse(trainData$diag_2R <= 519, "I", 
                          ifelse(trainData$diag_2R <= 579, "J", 
                          ifelse(trainData$diag_2R <= 629, "K", 
                          ifelse(trainData$diag_2R <= 679, "L", 
                          ifelse(trainData$diag_2R <= 709, "M", 
                          ifelse(trainData$diag_2R <= 739, "N", 
                          ifelse(trainData$diag_2R <= 759, "O", 
                          ifelse(trainData$diag_2R <= 779, "P", 
                          ifelse(trainData$diag_2R <= 799, "Q", 
                          ifelse(trainData$diag_2R <= 999, "R",
                          ifelse(trainData$diag_2R <= 9999,"EV",'NA')))))))))))))))))))



trainData$diag_3R <- ifelse(grepl('(V)|(E)',trainData$diag_3),'9999',trainData$diag_3)
trainData$diag_3R <- as.numeric(trainData$diag_3R)
trainData$diag_3R <- ifelse(trainData$diag_3R <= 139, "A", 
                          ifelse(trainData$diag_3R <= 239, "B", 
                          ifelse(trainData$diag_3R <= 279, "C", 
                          ifelse(trainData$diag_3R <= 289, "D", 
                          ifelse(trainData$diag_3R <= 319, "E", 
                          ifelse(trainData$diag_3R <= 359, "F", 
                          ifelse(trainData$diag_3R <= 389, "G", 
                          ifelse(trainData$diag_3R <= 459, "H", 
                          ifelse(trainData$diag_3R <= 519, "I", 
                          ifelse(trainData$diag_3R <= 579, "J", 
                          ifelse(trainData$diag_3R <= 629, "K", 
                          ifelse(trainData$diag_3R <= 679, "L", 
                          ifelse(trainData$diag_3R <= 709, "M", 
                          ifelse(trainData$diag_3R <= 739, "N", 
                          ifelse(trainData$diag_3R <= 759, "O", 
                          ifelse(trainData$diag_3R <= 779, "P", 
                          ifelse(trainData$diag_3R <= 799, "Q", 
                          ifelse(trainData$diag_3R <= 999, "R",
                          ifelse(trainData$diag_3R <= 9999,"EV",'NA')))))))))))))))))))
################################################################################
#------------------- Derive New Variables  -----------------------------------#

#No of Important Diagnosis
trainData$No_Diags <- apply(trainData[,1:3],1,function(x) sum(!is.na(x)))
round(prop.table(table(trainData$No_Diags,trainData$readmitted),margin=1),2)


#Hospice
hospicecode <- c("Hospice / home","Hospice / medical facility")
Hospice <- trainData[trainData$discharge_disposition_id %in% hospicecode,c('discharge_disposition_id','readmitted','No_Diags')]
#Check if patients who were sent to Hospice were not readmitted (possibily died)
round(prop.table(table(Hospice$discharge_disposition_id,Hospice$readmitted),margin = 1),2)
round(prop.table(table(Hospice$discharge_disposition_id,Hospice$No_Diags),margin = 1),2)

trainData$hospice <- ifelse(trainData$discharge_disposition_id %in% c("Hospice / home","Hospice / medical facility"), 1,0)

trainData$admDate <- strptime(trainData$admissionDate,format = "%d/%m/%Y")
trainData$admYear <- strftime(trainData$admDate,format = "%Y")
trainData$admMonth <- strftime(trainData$admDate,format = "%m")
trainData$admMonthYear <- strftime(trainData$admDate,format = "%m-%Y")
trainData$admWeekday <- strftime(trainData$admDate,format = "%u")
trainData$admWeekend <- ifelse(trainData$admWeekday >= 6,'Weekend','Weekday')

################################################################################
#------ #Check Zerow/Low Variance variables -----------------------------------#

nzv <- nearZeroVar(trainData,saveMetrics = TRUE)
print(nzv)
zerovariance <- rownames(nzv[nzv$zeroVar == TRUE,])
print(zerovariance)

#Exclude zerovariance
trainData <- trainData[,!names(trainData) %in% zerovariance]
str(trainData)
names(trainData)


################################################################################
#----------------------- Prepare trian data -----------------------------------#

#To exclude
diaDescription <- c('diag_1_desc','diag_2_desc','diag_3_desc')
trainData <- trainData[,!(names(trainData) %in% diaDescription)]


#Convert Character to Factor
trainData[sapply(trainData,is.character)] <- lapply(trainData[sapply(trainData,is.character)],as.factor)
str(trainData)

####################################################################
#--------------- Visualization ------------------------------------#

#Creates varialbes groups
profiles <- names(trainData)[1:3]
hospitals <- names(trainData)[4:11]

#Check proportion
table(trainData$readmitted)
percent <- round(prop.table(table(trainData$readmitted)),2)

sprintf("The proportion of target variable is:- Yes: %.1f percent and No: %.1f percent",percent[[1]]*100,percent[[2]]*100)


ggplot(data=trainData,aes(x=readmitted,fill=readmitted)) +
  geom_bar(stat='count',color='black',width = 0.7) +
  guides(fill = FALSE) +
  ylab("Total readmitted") +
  xlab("Readmitted (1=Yes,0=No)") +
  ggtitle("Readmitted vs. Non-readmitted")

mosaicplot(trainData$age ~ trainData$readmitted,
           main = "Readmitted by Age Groups",
           color = TRUE,
           shade = FALSE,
           xlab = "Age Groups",
           ylab = "Readmitted")

mosaicplot(~ gender + readmitted,
           data=trainData,
           main = "Readmitted by Gender",
           color = TRUE,
           shade = FALSE,
           xlab = "Gender",
           ylab = "Readmitted")

ggpairs(trainData[,c(profiles,'readmitted')])
ggpairs(trainData[,c(hospitals,'readmitted')])


ggplot(data=trainData,
       aes(x=readmitted,y=time_in_hospital)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("By Time in Hospital") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=num_lab_procedures)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No. of Lab Procedure") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=num_procedures)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Procedure") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=num_medications)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Medication") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=number_outpatient)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Outpatients") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=number_emergency)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Emergency") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=number_inpatient)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("Number of Inpatient") +
  theme_economist()

ggplot(data=trainData,
       aes(x=readmitted,y=number_diagnoses)) +
  geom_boxplot(aes(fill= readmitted)) +
  ggtitle("No of Diagnoses") +
  theme_economist()

#Factor variable Visualization
mosaicplot(~ max_glu_serum + readmitted,
           data=trainData,
           main = "max_glu_serum",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ A1Cresult + readmitted,
           data=trainData,
           main = "A1Cresult",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ metformin + readmitted,
           data=trainData,
           main = "metformin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ repaglinide + readmitted,
           data=trainData,
           main = "repaglinide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ nateglinide + readmitted,
           data=trainData,
           main = "nateglinide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ chlorpropamide + readmitted,
           data=trainData,
           main = "chlorpropamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glimepiride + readmitted,
           data=trainData,
           main = "glimepiride",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glipizide + readmitted,
           data=trainData,
           main = "glipizide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glyburide + readmitted,
           data=trainData,
           main = "glyburide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ tolbutamide + readmitted,
           data=trainData,
           main = "tolbutamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ pioglitazone + readmitted,
           data=trainData,
           main = "pioglitazone",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glyburide + readmitted,
           data=trainData,
           main = "glyburide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ tolbutamide + readmitted,
           data=trainData,
           main = "tolbutamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ pioglitazone + readmitted,
           data=trainData,
           main = "pioglitazone",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ rosiglitazone + readmitted,
           data=trainData,
           main = "rosiglitazone",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ acarbose + readmitted,
           data=trainData,
           main = "acarbose",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ miglitol + readmitted,
           data=trainData,
           main = "miglitol",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ tolazamide + readmitted,
           data=trainData,
           main = "tolazamide",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ insulin + readmitted,
           data=trainData,
           main = "insulin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glyburide.metformin + readmitted,
           data=trainData,
           main = "glyburide.metformin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ max_glu_serum + readmitted,
           data=trainData,
           main = "max_glu_serum",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ glipizide.metformin + readmitted,
           data=trainData,
           main = "glipizide.metformin",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ change + readmitted,
           data=trainData,
           main = "change",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ diabetesMed + readmitted,
           data=trainData,
           main = "diabetesMed",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")

mosaicplot(~ readmitted + readmitted,
           data=trainData,
           main = "readmitted",
           color = TRUE,
           shade = FALSE,
           ylab = "Readmitted")



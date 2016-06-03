set.seed(1)
N <- 20
dat <- data.frame( 
  x = factor(sample(LETTERS[1:5],N,replace=TRUE)),
  y = rnorm(N,5,12),
  z = rnorm(N,-5,17) + runif(N,2,12)
)

#' Function which wraps preProcess to exclude factors from the model.matrix
ppWrapper <- function( x, excludeClasses=c("factor"), ... ) {
  whichToExclude <- sapply( x, function(y) any(sapply(excludeClasses, function(excludeClass) is(y,excludeClass) )) )
  processedMat <- predict( preProcess( x[!whichToExclude], ...), newdata=x[!whichToExclude] )
  x[!whichToExclude] <- processedMat
  x
}

> ppWrapper(dat)

############################################################
rocCurve <- roc(response = churnTest$churn,
                predictor = gbmProbs[, "yes"],
                levels = rev(levels(churnTest$churn)))
rocCurve

numerics <- c("account_length", "total_day_calls", "total_night_calls")
## Determine means and sd's
procValues <- preProcess(churnTrain[,numerics],
                             + method = c("center", "scale", "YeoJohnson"))
## Use the predict methods to do the adjustments
trainScaled <- predict(procValues, churnTrain[,numerics])
testScaled <- predict(procValues, churnTest[,numerics])


############################################################


library(caret)
data(mtcars)

inTrain <- createDataPartition(mtcars$mpg,p=0.7,list=FALSE)

dev <- mtcars[inTrain,]
val <- mtcars[-inTrain,]

lmFit <- train(mpg ~.,data=dev,method='lm')
summary(lmFit)

ctrl <- trainControl(method='cv',number=10)
lmCVFit <- train(mpg ~.,data=mtcars,
                 method = 'lm',
                 trcontrol = ctrl,
                 metric = 'Rsquared')
summary(lmCVFit)

residuals <- resid(lmFit)
predictedValues <- predict(lmFit)
plot(dev$mpg,residuals)
abline(0,0)
plot(dev$mpg,predictedValues)


varImp(lmCVFit)
plot(varImp(lmFit))

predictedVal <- predict(lmFit,val)
modelValues <- data.frame(obs=dev$mpg,pred=predictedVal)
defaultSummary(modelValues)

#################################################################################
setwd("D:/Nung Lian Sume/R Version 3.3.0/leaf")

leaf <- read.csv(file='leaf.csv',header=FALSE)
leafAttributes <- c('Class','Specimen','Eccentricity','AspectRatio',
                'Elongation','Sodidity','StochasticConvexity','Isoperimetric',
                'MaxIndentationDepth','Lobedness','AvgIntensity','AvgContrast',
                'Smoothness','ThirdMoment','Uniformity','Entropy')
names(leaf) <- leafAttributes
leaf$Class <- as.factor(leaf$Class)
summary(leaf)

# `xyplot(resampls); dotplot(resampls); densityplot(resampls); bwplot(resampls); splom(resampls); parallelplot(resampls);`
# 
# Read more at `?xyplot.resamples`

ctrl <- trainControl(method='repeatedcv',number = 10,repeats=5,
                     selectionFunction = 'oneSE')

inTrain <- createDataPartition(leaf$Class,p = 0.75,list=FALSE)

trf <- train(Class ~.,data=leaf,method='rf',metric='Kappa',
             trControl = ctrl, subset = inTrain)

tgbm <- train(Class ~.,data=leaf,method = 'gbm',metric='Kappa',
              trControl = ctrl, subset = inTrain, verbose = FALSE)


resampls <- resamples(list(RF = trf,GBM = tgbm))

difValues <- diff(resampls)
summary(difValues)

xyplot(resampls)
dotplot(resampls)
densityplot(resampls)
bwplot(resampls)
splom(resampls)
parallelplot(resampls)


test <- leaf[-inTrain,]
test$pred.leaf.rf <- predict(trf,test,'raw')
confusionMatrix(test$pred.leaf.rf,test$Class)

varImp(trf,scale=FALSE)

#####################################################################################
library(caret)
library(dplyr)
library(kernlab)
library(pROC)


data("segmentationData")
trainIndex <- createDataPartition(segmentationData$Case,p=0.5,list=FALSE)
trainData <- segmentationData[trainIndex,]
testData <- segmentationData[-trainIndex,]
trainX <- trainData[,4:61]
sapply(trainX,summary)

set.seed(1492)
ctrl <- trainControl(method='repeatedcv',repeats = 5,number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

svm.tune <- train(x=trainX,
                  y=trainData$Class,
                  method = 'svmRadial',
                  tuneLength = 9,
                  preProc =c('center','scale'),
                  metric = 'ROC',
                  trControl = ctrl)

svm.tune

tunegrid <- expand.grid(sigma = c(0.01,0.015,0.2),
            C = c(0.75,0.9,1,1.1,1.25))

smv.tunegrid <- train(x=trainX,
                  y=trainData$Class,
                  method = 'svmRadial',
                  preProc = c('center','scale'),
                  metric = 'ROC',
                  tuneGrid = tunegrid,
                  trControl = ctrl)

rValues <- resamples(list(svm=svm.tune,smv.tunegrid))
rValues$values
summary(rValues)
bwplot(rValues,metric="ROC",ylab=c("linear kernel","radial kernel"))


####################################################################################33

library(caret)
library(mlbench)
data(Sonar)
set.seed(107)

inTrain <- createDataPartition(Sonar$Class,
                               p=0.75,
                               list=FALSE)
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
nrow(testing)

plsFit <- train(Class ~.,
                data=training,
                method = 'pls',
                preProc = c('center','scale'))


plsFit <- train(Class ~.,
                data = training,
                method = 'pls',
                tuneLength = 15,
                preProc = c('center','scale'))

ctrl <- trainControl(method = 'repeatedcv',
                     number = 10,
                     repeats = 3)

plsFit <- train(Class ~.,
                data = training,
                method = 'pls',
                tuneLength = 15,
                trControl = ctrl,
                preProc = c('center','scale'))

ctrl <- trainControl(method = 'repeatedcv',
                    repeats =3,
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

plsFit <- train(Class ~.,
                data = training,
                method = 'pls',
                tuneLength = 15,
                trControl = ctrl,
                metric = 'ROC',
                preProc = c('center','scale'))

plot(plsFit)

plsClasses <- predict(plsFit,newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit,newdata = testing,type='prob')
head(plsProbs)

confusionMatrix(data=plsClasses,testing$Class)


## To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)

rdaFit <- train(Class ~.,
                data = training,
                method = 'rda',
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")

rdaFit

rdaClasses <- predict(rdaFit,newdata = testing)
confusionMatrix(rdaClasses,testing$Class)

#Comparing their resampling results
resamps <- resamples(list(pls=plsFit,rda=rdaFit))
summary(resamps)

xyplot(resamps,what='BlandAltman')

#paired t-test can be used to compared the difference 
diffs <- diff(resamps)
summary(diffs)




#using random forests method 
setwd("C:/personal development/machine learning")
pmlTrain<-read.csv("pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
pmlTest<-read.csv("pml-testing.csv", header=T, na.string=c("NA", "#DIV/0!"))

## NA exclusion for all available variables
noNApmlTrain<-pmlTrain[, apply(pmlTrain, 2, function(x) !any(is.na(x)))] 
dim(noNApmlTrain)

## variables with user information, time and undefined
cleanpmlTrain<-noNApmlTrain[,-c(1:8)]
dim(cleanpmlTrain)

## 20 test cases provided clean info - Validation data set
cleanpmltest<-pmlTest[,names(cleanpmlTrain[,-52])]
dim(cleanpmltest)

#data cleaning
library(caret)
inTrain<-createDataPartition(y=cleanpmlTrain$classe, p=0.75,list=F)
training<-cleanpmlTrain[inTrain,] 
test<-cleanpmlTrain[-inTrain,] 

#Training and test set dimensions
dim(training)
dim(test)
library(caret)
set.seed(13333)
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
rffit<-train(classe~.,data=training, method="rf", trControl=fitControl2, verbose=F)
predrf<-predict(rffit, newdata=test)
confusionMatrix(predrf, test$classe)
pred20<-predict(rffit, newdata=cleanpmltest)

# Output for the prediction of the 20 cases provided
pred20
fitControl2<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
gmbfit<-train(classe~.,data=training, method="gbm", trControl=fitControl2, verbose=F)
gmbfit$finalModel
class(gmbfit)
predgmb<-predict(gmbfit, newdata=test)
confusionMatrix(predgmb, test$classe)
predtrain<-predict(gmbfit, newdata=training)
confusionMatrix(predtrain, training$classe)
predtrain<-predict(gmbfit, newdata=training)
confusionMatrix(predtrain, training$classe)
getwd()
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred20)
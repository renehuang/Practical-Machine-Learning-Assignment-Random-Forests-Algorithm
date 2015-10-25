setwd("C:/personal development/machine learning")
pmlTrain<-read.csv("pml-training.csv")
pmlTest<-read.csv("pml-testing.csv")

# simple comparison
nas <- colSums(is.na(pmlTrain));table(nas)
table(pmlTrain$user_name)
qplot(classe, data=pmlTrain, fill=user_name)


# Further exploratory analysis indicates that there are no outliers

install.packages(data.table)
library(data.table)
setInternet2(TRUE)
#Training data
url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
D<-fread(url)
#Test data
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
DTest<-fread(url)
#can not understand
isAnyMissing<-sapply(DTest,function(x)any(is.na(x)|x==''))
isPredictor<-!isAnyMissing&grepl('belt|[^(fore)]arm|dumbbell|forearm',names(isAnyMissing))
predCandidates<-names(isAnyMissing)[isPredictor]
predCandidates

varToInclude<-c('classe',predCandidates)
D<-D[,varToInclude,with=FALSE]
dim(D)

names(D)

D<-D[,classe:=factor(D[,classe])]
D[,.N,classe]

library(caret)
require(caret)

seed<-as.numeric(as.Date('2015-9-28'))
set.seed(seed)
inTrain<-createDataPartition(D$classe,p=0.6)
DTrain<-D[inTrain{1}]
DProbe<-D[-inTrain[1]]

x<-DTrain[,predCandidates,with=FALSE]
preProc<-preProcess(x)

XCS<-predict(preProc,X)
DTrainCS<-data.table(data.frame(classe=DTrain[,classe],XCS))

X<-DProbe[,predCandidates,with=FALSE]
XCS<-predict(preProc,X)
DProbeCS<-data.table(data.frame(classe=DProbe[,classe],XCS))

nzv<-nearZeroVar(DTrainCS,saveMetrics = TRUE)
if(any(nzv$nzv)) nzv else message('No variables with near zero variance')

histGroup<-function(data, regex){
  col<-grep(regex,names(data))
  col<-c(col,which(names(data)=='classe'))
  require(reshape2)
  
}
#1.Exploratory data analysis
Training_data=read.csv("D:/Coursera/Specialisation - Data Science/8.Machine Learning/Project/pml-training.csv")

##Data Frame structure & split 
dim(Training)
str(Training[1:15])
library("caret")
sample = createDataPartition(Training_data$classe, p = 3/4)[[1]]
Training = Training_data[sample,]
Testing = Training_data[-sample,]

##Only keep variables interesting
Training.clean<-Training[,8:159]

##Convertes all data to numeric
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],asNumeric))
integerNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.integer)],asNumeric))
Training.clean<-integerNumeric(Training.clean)
Training.clean<-factorsNumeric(Training.clean)
Training.clean<-Training.clean[,colSums(is.na(Training.clean))==0]

##Observed that most variables have NA value
na_count <-sapply(training.clean, function(y) sum(length(which(is.na(y)))))
plot(na_count);dim(training.clean)
training.clean<-training.clean[,colSums(is.na(training.clean))==0]
dim(training.clean)

##PCA analysis
pca <- prcomp(training.clean,center = TRUE,scale. = TRUE)
cumvar<-cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumvar, type="b", col="blue", pch=22, main = "PCA analysis",xlab="PC number added to model",ylab="Cummulative Variance Explained" )
q95<-which(cumvar>0.95);x<-c(-1,q95[1],q95[1]);y<-c(0.95,0.95,0)
lines(x,y,type="l", col="red");text(10,1,"0.95 variance", col="red");rm(q95,x,y)

#2.Create Prediction Model
library("caret")
model_rf<-train(Training$classe~., data=Training.clean, method="rf")
model_gbm<-train(Training$classe~., data=Training.clean, method="gbm")
model_lda<-train(Training$classe~., data=Training.clean, method="lda")

#3.Apply to testing set
##Clean Testing Set as performed for Training
Variables<-colnames(Training.clean)## Represent our set of variables used previously.
Testing.clean<-Testing[,Variables]
Testing.clean<-integerNumeric(Testing.clean)
Testing.clean<-factorsNumeric(Testing.clean)

pred_rf<-predict(model_rf,Testing.clean)
pred_gbm<-predict(model_gbm,Testing.clean)
pred_lda<-predict(model_lda,Testing.clean)

accuracy_rf = sum(pred_rf == Testing$classe) / length(pred_rf)
accuracy_gbm = sum(pred_gbm == Testing$classe) / length(pred_gbm)
accuracy_lda = sum(pred_lda == Testing$classe) / length(pred_lda)

results<-data.frame("lda"=accuracy_lda,"gbm"=accuracy_gbm,"rf"=accuracy_rf)
results
confusionMatrix(Testing$classe,predict(model_rf,Testing.clean))


# Application to Test data.
Test=read.csv("D:/Coursera/Specialisation - Data Science/8.Machine Learning/Project/pml-testing.csv")
pred_Test_rf=predict(model_rf,Test)
pred_Test_gbm=predict(model_gbm,Test)

table(pred_Test_rf,pred_Test_gbm)
pred_Test_rf

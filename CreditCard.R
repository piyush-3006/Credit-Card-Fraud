# Reading the data file

df<-read.csv(file.choose())
str(df)
plot(as.factor(df$Class))
table(df$Class)

# converting the class to factor type

df$Class<-as.factor(df$Class)

# Creating the time variable

df$time<-(ceiling(df$Time%/%3600)%%24)
df$time<-as.factor(df$time)
table(df$time,df$Class)

library(caTools)
summary(df)



#install.packages("DMwR")
#library(DMwR)

#install.packages("mlr")

#library(mlr)
#train.task<-makeClassifTask(data=train[,-1],target = "Class")
#test.task<-makeClassifTask(data=test[,-1],target="Class")

# undersampling the data

#train.under<-undersample(train.task,rate=0.1) # Keeping the majority class to 10%
#table(getTaskTargets(train.under))

# Oversampling the data

#train.over<-oversample(train.task,rate=15) # make majority class 15 times
#table(getTaskTargets(train.over))

# SMOTE<-Synthetic Minority Over-sampling Technique

#train.smote <- smote(train.task,rate = 15,nn = 5)
#listLearners("classif","twoclass")[c("class","package")]

install.packages("unbalanced")
library(unbalanced)
df$Class<-as.factor(df$Class)

#Balance the Dataset using ubSMOTE

data<-ubBalance(X=df[,-31],Y=df[,31],type="ubSMOTE", percOver=1000, percUnder=400, verbose=TRUE)
View(data)

# Create balanced data

balancedData<-cbind(data$X,data$Y)
table(balancedData$`data$Y`)

colnames(balancedData)[32]<-"Class"
colnames(balancedData[32])


str(balancedData)
class(balancedData)

# Scaling the data to remove the effect of amount

data_scale<-apply(balancedData[,-c(1,31,32)],2,scale)
data_scale<-as.data.frame(data_scale)

data_scale$Time<-balancedData$time
data_scale$Class<-balancedData$Class

View(data_scale)
str(data_scale)
str(data_scale)
colnames(data_scale)

# Removing the time column and spliting the data into train and test

data_scale<-data_scale[,-30]

split<-sample.split(data_scale$Class,0.7)
train_scale<-data_scale[split==T,]
test_scale<-data_scale[split==F,]

# Building the model

model<-glm(Class~.,data=train_scale,family="binomial")
summary(model)
model_new<-model<-glm(Class~.-V15,data=train_scale,family="binomial")
summary(model_new)
model_new1<-model<-glm(Class~.-V15-V21,data=train_scale,family="binomial")
summary(model_new1)
model_new2<-model<-glm(Class~.-V15-V21-V27,data=train_scale,family="binomial")
summary(model_new2)

# Building the classification matrix with cutoff 0.7

p<-predict(model_new2,test_scale,type="response")
table(test_scale$Class,p>0.7)

#  Calculating AUC Values

library(ROCR)
p1<-prediction(p,test_scale$Class)
as.numeric(performance(p1,"auc")@y.values)
perf=performance(p1,"tpr","fpr")
pred<-ifelse(p>0.7,1,0)
plot(perf)
library(caret)

# Confusion Matrix for Logistic Regression

confusionMatrix(test_scale$Class,pred)

# Training a SVM Model

library(e1071)
model_svm<-svm(Class~.,data=train_scale)
pred_svm<-predict(model_svm,newdata=test_scale)
table(pred_svm)

# Confusion Matrix for SVM Model
table(test_scale$Class,pred_svm)


setwd("C:/Users/MADHURMD/Downloads/date your data")
intern=read.csv("internship.csv")
stud=read.csv("student.csv")
test=read.csv("test.csv")
train=read.csv("train.csv")

#data cleaning
str(train)

train_merged<-merge(train, intern, by.x="Internship_ID", by.y="Internship_ID")
train_merged$Earliest_Start_Date=as.numeric(train$Earliest_Start_Date)
train_merged=train_merged[,-6] #removed the preferred location variable
test_merged=test[,-6] #removed the preferred location variable

test_merged$Earliest_Start_Date=as.numeric(test$Earliest_Start_Date)
test_merged<-merge(test_merged, intern, by.x="Internship_ID", by.y="Internship_ID")

###Initial random forest only on test and train
#library(randomForest)
#fit <- randomForest(train_merged$Is_Shortlisted ~ ., train,ntree=500)

#Fit the values
#predicted= predict(fit,test) #Did not work


#Tried xgboost
library(data.table)
library(plyr)
library(xgboost)

for(i in 1:ncol(train_merged)){
  train_merged[,i] <- as.numeric(train_merged[,i])
}

set.seed(23)
train_resp=train_merged$Is_Shortlisted
train_merged1=train_merged[,-7]
model_xgb <- xgboost(as.matrix(train_merged1), as.matrix(train_resp), objective="multi:softprob", num_class=2, nrounds=130, eta=0.1, max_depth=6, subsample=0.9, colsample_bytree=0.9, min_child_weight=1, eval_metric='merror')
summary(model_xgb)
#Fit the values
for(i in 1:ncol(test_merged)){
  test_merged[,i] <- as.numeric(test_merged[,i])
}

predicted= predict(model_xgb,as.matrix(test_merged))
pred=data.frame(matrix(predicted,ncol=2,byrow=TRUE))
pred[,1]=ifelse(pred[,1]>0.7,0,1)
write.csv(pred,"pred.csv")
fit

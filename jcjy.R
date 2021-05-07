library(pROC)
library(e1071)
library(caret)
num=0
set.seed(1)
require(caret)
auc_value<-as.numeric()
folds <- createFolds(y=svm_train[,17],k=6)
svm_train[,17]<-as.numeric(svm_train[,17])
for(i in 1:6)
{
  fold_test<-svm_train[folds[[i]],]
  fold_train<-svm_train[-folds[[i]],]
  fold_pre<-svm(score~.,data=fold_train,cost=1,gamma=0.125,probability=TRUE)
  fold_predict<-predict(fold_pre,newdata=fold_test,probability=TRUE)
  auc_value<- append(auc_value,as.numeric(auc(as.numeric(fold_test[,17]),fold_predict)))
}
num<-1
med2<-median(auc_value)
print(auc_value)
fold_test<-svm_train[folds[[num]],]
fold_train<-svm_train[-folds[[num]],]
fold_pre <- svm(score~.,data=fold_train,type="C-classification",cost=1,gamma=0.125,probability=TRUE)
fold_predict <- predict(fold_pre,newdata=fold_test,probability= TRUE)
fold_pro_predict<-attributes(fold_predict)$probabilities[,2]
roc_curve6<- roc(fold_test$score,as.numeric(fold_pro_predict))
plot(roc_curve6, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
      main="ROC curve for the set with middle AUC value")
tn<-0
tp<-0
fn<-0
fp<-0
for(i in 1:nrow(fold_test))
{
  if(fold_test$score[i]==1)
  {
    if(fold_predict[i]==1){tp<-tp+1}else{fp<-fp+1}
  }else{
    if(fold_predict[i]==1){fn<-fn+1}else{tn<-tn+1}
  }
}
conc_svm_cost1<-cbind(c("accuracy","recall","precision","F_measure"),c((tp+tn)/(tp+tn+fp+fn),tp/(tp+fn),tp/(tp+fp),2*(tp/(tp+fp))*(tp/(tp+fn))/(tp/(tp+fp)+tp/(tp+fn))))



cross6<-roc_curve6
cross8<-roc_curve8
cross10
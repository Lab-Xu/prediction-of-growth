library("party")
library("rpart")
library("ggplot2")
library("pROC")
svm_train[,17]=as.numeric(svm_train[,17])
svm_train[,17]<-svm_train[,17]-1
model<-glm(score~.,data=svm_train,family=binomial(link='logit')    )
summary(model)
model2<-step(object = model,trace = 0)
?step
summary(model2)

prob<-predict(object =model,newdata=svm_test,type = "response")
pred<-ifelse(prob>=0.49,1,0)
anova(object = model,test = "Chisq")
f<-table(svm_test$score,pred)
f
roc_curve_glm <- roc(svm_test$score,prob)
plot(roc_curve_glm , print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, print.thres=TRUE,main='logistic')


tn<-0
tp<-0
fn<-0
fp<-0
for(i in 1:nrow(svm_test))
{
  if(svm_test$score[i]==1)
  {
    if(pred[i]==1){tp<-tp+1}else{fp<-fp+1}
  }else{
    if(pred[i]==1){fn<-fn+1}else{tn<-tn+1}
  }
}
conc_svm_cost1<-cbind(c("accuracy","recall","precision","F_measure"),c((tp+tn)/(tp+tn+fp+fn),tp/(tp+fn),tp/(tp+fp),2*(tp/(tp+fp))*(tp/(tp+fn))/(tp/(tp+fp)+tp/(tp+fn))))











ctr=ctree(score~.,data=svm_train)
plot(ctr)
dtree<-rpart(score~.,data=svm_train)
printcp(dtree)
tree<-prune(dtree,cp=0.0125)
printcp(tree)
prob2<-predict(object =dtree,newdata=svm_test)
prob<-predict(object =ctr,newdata=svm_test)
pred<-ifelse(prob>=mean(prob),1,0)
table(svm_test$score,pred,dnn=c("zhenshizhi","yucezhi"))
roc_curve_dtree <- roc(svm_test$score,prob2)
plot(roc_curve_dtree, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE, print.thres=TRUE,main='tree')
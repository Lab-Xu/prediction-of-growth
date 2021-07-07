library("psych")#主成分分析
library("e1071")
library("pROC")
library("ROCR")
library("ggplot2")
cx18 <- na.omit(cx18)
cl <- kmeans(df,4); cl
#2017
cz_operate_2017<-cz_2017
cz_operate_2017<-cz_operate_2017[-c((which(cz_operate_2017[,3]=="地方国有企业")),(which(cz_operate_2017[,3]=="国有企业")),(which(cz_operate_2017[,3]=="中央国有企业")),(which(cz_operate_2017[,3]=="集体企业"))),]
cz_operate_2017<-na.omit(cz_operate_2017)
num_2017<-cz_operate_2017[,1]
cz_operate_2017<-as.matrix(cz_operate_2017[,c(c(4:10),c(12:20))])
for(i in 1:16)
{
  cz_operate_2017[,i]<-scale(cz_operate_2017[,i])#scale即为标准化
}
pca_2017<-princomp(cz_operate_2017,cor=FALSE)
summary(pca_2017)
pca_2017<-principal(cz_operate_2017,nfactors=10)
print(pca_2017)
cz_operate_2017<-cbind(cz_operate_2017,0)
for(i in 1:nrow(cz_operate_2017))
{
  for(j in 1:10)
  {
    cz_operate_2017[i,17]<-cz_operate_2017[i,17]+(pca_2017$Structure[,j]%*%cz_operate_2017[i,c(1:16)])*pca_2017$Vaccounted[2,j]
  }
  
}

#2018
cz_operate_2018<-cz_2018
cz_operate_2018<-cz_operate_2018[-c((which(cz_operate_2018[,3]=="地方国有企业")),(which(cz_operate_2018[,3]=="国有企业")),(which(cz_operate_2018[,3]=="中央国有企业")),(which(cz_operate_2018[,3]=="集体企业"))),]
cz_operate_2018<-na.omit(cz_operate_2018)
cz_operate_2018[,6]<--cz_operate_2018[,6]
num_2018<-cz_operate_2018[,1]
cz_operate_2018<-as.matrix(cz_operate_2018[,c(c(4:10),c(12:20))])
for(i in 1:16)
{
  cz_operate_2018[,i]<-scale(cz_operate_2018[,i])
}
pca_2018<-princomp(cz_operate_2018,cor=FALSE)
summary(pca_2018)
pca_2018<-principal(cz_operate_2018,nfactors=10)
print(pca_2018)
cz_operate_2018<-cbind(cz_operate_2018,0)
for(i in 1:nrow(cz_operate_2018))
{
  for(j in 1:10)
  {
    cz_operate_2018[i,17]<-cz_operate_2018[i,17]+(pca_2018$Structure[,j]%*%cz_operate_2018[i,c(1:16)])*pca_2018$Vaccounted[2,j]
  }
  
}
#2016
#combine 2016 and 2017 and their score
cz_operate_2016<-cz_2016
cz_operate_2016<-na.omit(cz_operate_2016)
num_2016<-cz_operate_2016[,1]
cz_operate_2016<-as.matrix(cz_operate_2016[,c(c(4:10),c(12:20))])
for(i in 1:16)
{
  cz_operate_2016[,i]<-scale(cz_operate_2016[,i])
}
pca_2016<-princomp(cz_operate_2016,cor=FALSE)
summary(pca_2016)
pca_2016<-principal(cz_operate_2016,nfactors=10)
print(pca_2016)
table(pca_2016$Structure)
cz_operate_2016<-cbind(cz_operate_2016,0)
for(i in 1:nrow(cz_operate_2016))
{
  for(j in 1:10)
  {
    cz_operate_2016[i,17]<-cz_operate_2016[i,17]+(pca_2016$Structure[,j]%*%cz_operate_2016[i,c(1:16)])*pca_2016$Vaccounted[2,j]
  }
  
}
cz_operate_2016<-cbind(cz_operate_2016,0)
for(i in 1:nrow(cz_operate_2016))
{
  cz_operate_2016[i,18]<-cz_operate_2017[match(as.matrix(num_2016)[i],as.matrix(num_2017)),17]#将17年的标签放到16年
}
cz_operate_2016<-na.omit(cz_operate_2016)
colnames(cz_operate_2016)[18]<-"score"
bishe_2016=cz_operate_2016
bishe_2016<-bishe_2016[,-17]
cz_operate_2017<-cbind(cz_operate_2017,0)
for(i in 1:nrow(cz_operate_2017))
{
  cz_operate_2017[i,18]<-cz_operate_2018[match(as.matrix(num_2017)[i],as.matrix(num_2018)),17]
}
colnames(bishe_2017)[17]<-"score"
colnames(cz_operate_2017)[18]<-"score"
cz_operate_2017<-na.omit(cz_operate_2017)
#2015
#combine 2016 and 2015
cz_operate_2015<-cz_2015
cz_operate_2015<-na.omit(cz_operate_2015)
num_2015<-cz_operate_2015[,1]
cz_operate_2015<-as.matrix(cz_operate_2015[,c(c(4:10),c(12:20))])
for(i in 1:16)
{
  cz_operate_2015[,i]<-scale(cz_operate_2015[,i])
}
cz_operate_2015<-cbind(cz_operate_2015,0)
for(i in 1:nrow(cz_operate_2015))
{
  cz_operate_2015[i,17]<-cz_operate_2016[match(as.matrix(num_2015)[i],as.matrix(num_2016)),17]
}
cz_operate_2015<-na.omit(cz_operate_2015)
cz_operate_2017<-cz_operate_2017[,-17]
colnames(cz_operate_2015)[17]<-"score"
svr_all<-rbind(cbind(cz_operate_2015,"2015"),cbind(cz_operate_2016,"2016"),cbind(cz_operate_2017,"2017"))
svr_all<-data.frame(svr_all,stringsAsFactors = FALSE)#数据框
for(i in 1:16)
{
  svr_all[,i]<-as.numeric(svr_all[,i])
}

#divide train and test
set.seed(1)#设置随机数种子
sub<-sample(1:nrow(svm_all),round(nrow(svm_all)*1/5))#测试集
svm_train<-svr_all[-sub,]
svm_train<-na.omit(svm_train)
svm_test<-svr_all[sub,]
svm_test<-na.omit(svm_test)


#compute median of 2016 and 2017 score
med_train_2016<-median(as.numeric(svm_train[which(svm_train$V18=="2016"),17]))#标签处理需要把训练集和测试集分开
med_train_2017<-median(as.numeric(svm_train[which(svm_train$V18=="2017"),17])) 
med_train_2015<-median(as.numeric(svm_train[which(svm_train$V18=="2015"),17]))


#score>median,score=1,else,score=0
for(i in 1:nrow(svm_train))
{
  if(svm_train[i,18]=="2016")
  {
    if(as.numeric(svm_train[i,17])>med_train_2016)
    {
      svm_train[i,17]="1"
    }else
    {
      svm_train[i,17]="0"
    }
  }else if(svm_train[i,18]=="2017")
  {
    if(as.numeric(svm_train[i,17])>med_train_2017)
    {
      svm_train[i,17]="1"
    }else
    {
      svm_train[i,17]="0"
    }
  }else
  {
    if(as.numeric(svm_train[i,17])>med_train_2015)
    {
      svm_train[i,17]="1"
    }else
    {
      svm_train[i,17]="0"
    }
  }
}
svm_train<-svm_train[,-18]
model_c<-svm(score~.,data=svm_train,type="C-classification",cross=10,probability =TRUE)
print(model_c$accuracies)
summary(model_c)
#operate test same as train
med_test_2016<-median(as.numeric(svm_test[which(svm_test$V18=="2016"),17]))
med_test_2017<-median(as.numeric(svm_test[which(svm_test$V18=="2017"),17]))
med_test_2015<-median(as.numeric(svm_test[which(svm_test$V18=="2015"),17]))


for(i in 1:nrow(svm_test))
{
  if(svm_test[i,18]=="2016")
  {
    if(as.numeric(svm_test[i,17])>med_test_2016)
    {
      svm_test[i,17]="1"
    }else 
    {
      svm_test[i,17]="0"
    }
  }else if (svm_test[i,18]=="2017")
  {
    if(as.numeric(svm_test[i,17])>med_test_2017)
    {
      svm_test[i,17]="1"
    }else
    {
      svm_test[i,17]="0"
    }
  }else
  {
    if(as.numeric(svm_test[i,17])>med_test_2015)
    {
      svm_test[i,17]="1"
    }else
    {
      svm_test[i,17]="0"
    }
  }
}
svm_test<-svm_test[,-18]

#predict and operate the conclusion
svm_p<-predict(model_c,newdata =svm_test,probability = TRUE)
svm_pro<-attributes(svm_p)$probabilities[,2]
tn<-0
tp<-0
fn<-0
fp<-0
for(i in 1:nrow(svm_test))
{
  if(svm_test$score[i]==1)
  {
    if(svm_p[i]==1){tp<-tp+1}else{fp<-fp+1}
  }else{
    if(svm_p[i]==1){fn<-fn+1}else{tn<-tn+1}
  }
}
conc_svm<-cbind(c("accuracy","recall","precision","F_measure"),c((tp+tn)/(tp+tn+fp+fn),tp/(tp+fn),tp/(tp+fp),2*(tp/(tp+fp))*(tp/(tp+fn))/(tp/(tp+fp)+tp/(tp+fn))))
svm_roc <- roc(svm_test$score,as.numeric(svm_pro))
plot(svm_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='SVM模型ROC曲线 kernel = radial')
print(conc_svm)


#modificate parameter
model_c5<-svm(score~.,data=svm_train,type="C-classification",cost=1,gamma=0.125,probability =TRUE)
svm_p_cost1<-predict(model_c5,newdata =  svm_test,probability = TRUE)
svm_pro_cost1<-attributes(svm_p_cost1)$probabilities[,2]
tn<-0
tp<-0
fn<-0
fp<-0
for(i in 1:nrow(svm_test))
{
  if(svm_test$score[i]==1)
  {
    if(svm_p_cost1[i]==1){tp<-tp+1}else{fp<-fp+1}
  }else{
    if(svm_p_cost1[i]==1){fn<-fn+1}else{tn<-tn+1}
  }
}





conc_svm_cost1<-cbind(c("accuracy","recall","precision","F_measure"),c((tp+tn)/(tp+tn+fp+fn),tp/(tp+fn),tp/(tp+fp),2*(tp/(tp+fp))*(tp/(tp+fn))/(tp/(tp+fp)+tp/(tp+fn))))
svm_roc_cost1 <- roc(svm_test$score,as.numeric(svm_pro_cost1))
p1<-ggroc(list(AUC_0.901_k_6_=cross6,AUC_0.904_k_8_=cross8,AUC_0.896_k_10_=cross10),legacy.axes = TRUE)+ 
  xlab("1-Specificity") + 
  ylab("Sensitivity")+
  theme(legend.position =c(0.78,0.3))


p1<-p1+ 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))

?geom_text
?ggroc



X<-ggroc(list(AUC_0.892_SVM_=svm_roc_cost1,AUC_0.886_logistic_=roc_curve_glm,AUC_0.835_k_neighbor_=roc_curve_knn,AUC_0.781_nb_=roc_curve_nb,AUC_0.864_dtree_=roc_curve_dtree),legacy.axes = TRUE)+ 
  xlab("1-Specificity") + 
  ylab("Sensitivity")+
  theme(legend.position = c(0.845,0.3))

X<-X+
  theme(panel.background = element_blank(),axis.line = element_line(colour="black"))


y<-ggroc(list(AUC_0.892=svm_roc_cost1),legacy.axes = TRUE)+ 
  xlab("1-Specificity") + 
  ylab("Sensitivity")+
  theme(legend.position = c(0.845,0.3))
y<-y+
  theme(panel.background = element_blank(),axis.line = element_line(colour="black"))


p<-ggroc(list(AUC_0.901_k_6_=cross6,AUC_0.904_k_8_=cross8,AUC_0.896_k_10_=cross10),legacy.axes = TRUE)+ 
  xlab("1-Specificity") + 
  ylab("Sensitivity")+
  theme(legend.position =c(0.78,0.3))


p<-p+ 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"))




plot(svm_roc_cost1, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),grid.col=c("green", "red"), print.thres=TRUE,reuse.auc=FALSE,max.auc.polygon=TRUE,main='成长指数SVM模型ROC曲线,gamma=0.125,cost=1')
svm_train[,17]<-as.factor(svm_train[,17])
tobk<-tune.svm(score~.,data=svm_train,type="C-classification",cost=c(0.0625,0.125,0.25,0.5,1,2,4,8,16),gamma=c(0.0625,0.125,0.25,0.5,1,2,4,8,16),probability=TRUE)
plot(tobk,xlab=expression(gamma),ylab="损失惩罚参数C",main="成长指数不同参数组合下的预测错误率",nlevels=10,color.palette=terrain.colors)





























q<-ggplot()+geom_point(aes(x=1-svm_roc_cost1$specificities,y=svm_roc_cost1$sensitivities),color="red")+
  geom_point(aes(x=1-roc_curve_glm$specificities,y=roc_curve_glm$sensitivities),color="green")












































TPRandFPR<-function(pro,real,gap)
{ 
  pro<-as.numeric(pro)
  pre<-rep(0,length(pro))
  tn<-0
  tp<-0
  fn<-0
  fp<-0
  for(i in 1:length(pro))
  {
    if(pro[i]>gap)
    {
      pre[i]<-1
    }
  }
  for(i in 1:length(real))
  {
    if(real[i]==1)
    {
      if(pre[i]==1){tp<-tp+1}else{fp<-fp+1}
    }else{
      if(pre[i]==1){fn<-fn+1}else{tn<-tn+1}
    }
  }
  return(c(tp/(tp+fp),fn/(fn+tn)))
}
ROCmatrix<-function(pro,real,dot)
{
  ROC<-data.frame(nrow=dot,ncol=1,stringsAsFactors = FALSE)
  for(i in 0:dot-1)
  {
    ROC[i,]<-TPRandFPR(pro,real,i/dot)
  }
  return(ROC)
}
rocSVM<-cbind(ROCmatrix(attributes(pred1)$probabilities[,2],as.numeric(svm_test_f22[,838])-1,100),"SVM")
colnames(rocSVM)<-c("Sensitivity","1-Specificity","model")
rocRF<-cbind(ROCmatrix(rf_model1$test$votes[,2],as.numeric(svm_test_f22[,838])-1,100),"RF")
colnames(rocRF)<-c("Sensitivity","1-Specificity","model")
ROC<-rbind(rocSVM,rocRF)
ggplot(ROC,aes(x=Sensitivity,y=1-Specificity,colour=model))+geom_line()

write.table(cxsummm,file="D:/cxsummm.csv")
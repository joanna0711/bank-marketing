library(ISLR)
library(MASS)
library(class)
library(leaps)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(xlsx)
library(MASS)

library(caret)
library(randomForest)

data_csv <- read.csv("/Users/song-wonjeong/Desktop/final_data_bank.csv", header=TRUE) 
boxplot(data)$stats

data_csv <- data_csv[-1] #전처리과정에서 생긴 순서 열 제거
View(data_csv)

str(data_csv)

######train/test 비율로 나누기 ###
train=sample(1:nrow(data_csv),nrow(data_csv)/2)
test=data_csv[-train,]




############################## test / train 개수로 나누기 #############
shuffled_data=data_csv[sample(1:nrow(data_csv)),] #데이터 섞기
shuffled_data[,"idx"]<-c(1:nrow(data_csv))  
attach(shuffled_data)
dim(data_csv) #행과 열의 개수

train=(idx<=2000)  #트레인데이터 2000개 
shuffled_data.test=shuffled_data[!train,]
dim(shuffled_data.test)
Y.test=Y[!train]

Y.test   #테스트데이터 574개



########### LOOCV 교차 검증


library(boot)
glm.fit=glm(Y~job+contact+month+duration+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,data=data_csv)
cv.err=cv.glm(data_csv,glm.fit)
cv.err$delta



cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(Y~job+contact+month+duration+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,data=data_csv)
  cv.error[i]=cv.glm(data_csv,glm.fit)$delta[1]
}

cv.error


################ k-fold 교차검증

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(Y~job+contact+month+duration+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx,data=data_csv)
  cv.error.10[i]=cv.glm(data_csv,glm.fit,K=10)$delta[1]
}
cv.error.10
mean(cv.error.10)



################# 부트스트랩
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/var(X)+var(Y)-2*cov(X,Y))
}

alpha.fn(data_csv,1:100)

###############배깅과 랜덤포레스트 
library(randomForest)
set.seed (1)
bag.data_csv=randomForest(as.factor(Y)~.,data=data_csv,subset=train,mtry=21,importance=TRUE) ##as?벡터값변경
bag.data_csv

yhat.bag =predict(bag.data_csv,newdata=data_csv[-train,])
data_csv.test=data_csv[-train ,"Y"]
plot(yhat.bag,data_csv.test)
abline(0,1)
mean((yhat.bag-data_csv.test)^2)

bag.data_csv=randomForest(Y~.,data=data_csv,subset=train, mtry=21,ntree=25)
yhat.bag = predict(bag.data_csv ,newdata=data_csv[-train ,])
mean((yhat.bag-test)^2) 

set.seed (1)
rf.data_csv=randomForest(Y~.,data=data_csv,subset=train,
                         mtry=6,importance =TRUE) #의문? 
yhat.rf = predict(rf.data_csv ,newdata=data_csv[-train ,])
mean((yhat.rf-test)^2)

importance (rf.data_csv)

varImpPlot (rf.data_csv)

########### 부스팅 ###############3
library(gbm)
set.seed (1)
boost.data_csv=gbm(Y~.,data=data_csv[train,],distribution=
                     "gaussian ",n.trees=5000,interaction.depth=4)

summary(boost.data_csv)

par(mfrow=c(1,2))
plot(boost.data_csv,i="rm")
plot(boost.data_csv,i="lstat")

yhat.boost=predict(boost.data_csv,newdata=data_csv[-train,],n.trees=5000)
mean((yhat.boost-test)^2)

boost.data_csv=gbm(Y~.,data=data_csv[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.data_csv,newdata=data_csv[-train,],n.trees=5000)
mean((yhat.boost-test)^2)

############서포트 벡터 머신 ###############





#################### random forest 
temp=table(data_csv$Y) 
prop.table(temp)


divide =sample(c(rep(0,0.7*nrow(iris)),rep(1,0.3*nrow(iris))))
train=data_csv[divide==0,]
test=data_csv[divide==1,]
temp=table(train$class)
prop.table(temp)

rf.tree =randomForest(Y~.,data=data_csv)
rf.tree

rf.tree = randomForest(Y~.,data=train)
rf.tree

pred=predict(rf.tree,test)
confusionMatrix(table(pred,test$Y))




##########################




install.packages("randomForest")
library(randomForest)

train$Y <- as.factor(train$Y)
set.seed(1)

rt_tree <- randomForest(as.factor(Y) ~ ., data=train, importance=TRUE)
rt_tree

save(rt_tree, file = "/Users/song-wonjeong/Desktop/final_data_bank.csv")

load("/Users/song-wonjeong/Desktop/final_data_bank.csv")
rt_tree

rf_pred<- predict(rt_tree, train)

importance(rf.tree)
varImpPlot(rf.tree)

#check up
conMat<- confusionMatrix(RF_prediction1, train$Survived)
conMat$table


rf.tree = randomForest(Y~., data = data_csv, subset = train, mtry=4, importance = TRUE)
yhat.rf = predict(rf.tree, newdata = data_csv[-train,])
mean((as.numeric(unlist(yhat.rf))-as.numeric(unlist(data_csv.test)))^2)
importance(rf.data)
varImpPlot(rf.data)

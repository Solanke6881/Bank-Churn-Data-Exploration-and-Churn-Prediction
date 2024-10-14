data=read.csv("C:\\Users\\admin\\Downloads\\15aprildata.csv")
##SVM
library(readxl)
library(e1071)
library(caret)

n=nrow(data);n
set.seed(100000)
train_index=sample(1:n,0.8*n,replace = FALSE)
train_data=DATA[train_index,]
test_data=DATA[-train_index,]
model=svm(Churn~.,data=train_data,kernel="linear");model
pre=predict(model,test_data)
cm=confusionMatrix(as.factor(test_data$Churn),pre);cm

n=nrow(data);n
set.seed(100000)
train_index=sample(1:n,0.8*n,replace = FALSE)
train_data=data[train_index,]
test_data=data[-train_index,]
model=svm(Attrition_Flag~.,data=train_data,kernel="linear");model
pre=predict(model,test_data)
cm=confusionMatrix(as.factor(test_data$Attrition_Flag),pre);cm



##KNN
library(readxl)
library(class)
library(caret)
data=read_excel(file.choose())
n=nrow(data);n
set.seed(100000)
k=3
predict_Attrition_Flag=knn(train_data[,-1],test_data[,-1],train_data$Attrition_Flag,k)
cm1=confusionMatrix(predict_Attrition_Flag,as.factor(test_data$Attrition_Flag));cm1



##Naive bayes
model=naiveBayes(train_data[,-1],train_data[,1]);model
pre=predict(model,test_data[,-1])
cm3=confusionMatrix(as.factor(test_data$Attrition_Flag ),pre)
cm3

##Logistic Regression
data$Avg_Open_To_Buy=scale(data$Avg_Open_To_Buy)
model=glm(Attrition_Flag~.,data=data[,-15])

summary(model)

#Forward selection
library(MASS)
initial_modelF=glm(formula = Attrition_Flag ~ 1, data = test_data, family = binomial)
final_modelF=stepAIC(initial_modelF, direction = "forward", scope = list(upper = ~., lower = ~1), trace = TRUE)
summary(final_modelF)

#backward selection
initial_modelB=glm(formula = Attrition_Flag ~ ., data = train_data, family = binomial)
final_modelB=stepAIC(initial_modelB, direction = "backward", trace = TRUE)
summary(final_modelB)


##randomforest
#install.packages("randomForest")
library(randomForest)
model=randomForest(Attrition_Flag~.,train_data,ntree=100,importance=TRUE);model
PRE3=predict(model,test_data);PRE3
cm4=confusionMatrix(as.factor(test_data$Attrition_Flag),PRE3);cm4


accuracy=mean(PRE3==test_data$Attrition_Flag);accuracy

confusionMatrix(PRE3,test_data$Attrition_Flag)

cor(data)

lm=glm(Attrition_Flag~Gender,data=train_data,family= "binomial");lm
pred=predict(lm,test_data);pred
m=median(pred);m
PRED=ifelse(pred>m,'1','0')
plot(pred)
confusionMatrix(PRED,test_data$Attrition_Flag)

##PCA
pca_model=prcomp(data[,-1],scale.=TRUE)
summary(pca_model)
plot(pca_model,type="l")

prop_var=pca_model$sdev^2 / sum(pca_model$sdev^2);prop_var
plot(prop_var, type = "b", xlab = "Principal Component", ylab = "Proportion of Variance Explained", main = "Scree Plot")
biplot(pca_model)



install.packages("DMwR")
library(DMwR)
library(ROSE)
install.packages("smotefamily")
library(smotefamily)
oversampled_data=ovun.sample(Attrition_Flag~.,data,method="over",N=15000,seed=12345)
plot(oversampled_data)
nrow(oversampled_data)
o_s=SMOTE(Attrition_Flag~.,data,perc.over=100,k=5)

###Random_Oversampling
dim(train_data)
D1=train_data[,1]
w1=which(D1==0)
w2=which(D1==1)

D11=train_data[w1,] ##Dataset with not churn
dim(D11)


D12=train_data[w2,]  ##dataset with churn
dim(D12)

table(train_data$Attrition_Flag)
r1=sample(1:6824,4050)
DD1=D11[r1,]      


r2=sample(1:1277,4050,replace=TRUE)
DD2=D12[r2,]
View(DD2)           #oversampling from dataset with churn

DDF=rbind(DD1,DD2)
View(DDF)

set.seed(80100)
R=sample(1:8100,8100);R
DDF1=DDF[R,]
table(DDF1$Attrition_Flag)
View(DDF1)

A1=write.csv(DDF1,"15aprildata.csv")




####Decision Tree

install.packages("rpart")
library(rpart)
tree_model=rpart(Attrition_Flag ~ ., data = data, method = "class")
plot(tree_model)
text(tree_model, use.n = TRUE)


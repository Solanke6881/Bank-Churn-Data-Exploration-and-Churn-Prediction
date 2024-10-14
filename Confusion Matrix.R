
library(readxl)
data=read_excel(file.choose())
n=nrow(data)
set.seed(100000)
train_index=sample(1:n,0.8*n)
train_data=data[train_index,]
test_data=data[-train_index,]

dim(train_data)
D1=train_data[,1]
w1=which(D1==1)
w2=which(D1==0)

D11=train_data[w1,]    # Dataset with not churn
View(D11)
dim(D11)

D12=train_data[w2,]     # Dataset with churn 
View(D12)
dim(D12)
table(train_data$Churn)
data1=r$data
table(data1$Churn)
r1=sample(1:6810,4048)
DD1=D11[r1,]             ## sampling from dataset not churn
View(DD1)



r2=sample(1:1286,4048,replace=TRUE)
DD2=D12[r2,]
View(DD2)               ## Oversampling from dataset with churn 

DDF=rbind(DD1,DD2)
View(DDF)
nrow(DDF)
dim(DDF)

set.seed(8096)
R=sample(1:8096,8096);R
DDF1=DDF[R,]
View(DDF1)
A1=write.csv(DDF1,"karanshende.csv")
library(e1071)
library(caret)
colnames(DDF1)
model=naiveBayes(DDF1[,-1],DDF1[,1]);model
pre=predict(model,test_data[,-1]);pre
confusionMatrix(as.factor(test_data$Churn),pre)




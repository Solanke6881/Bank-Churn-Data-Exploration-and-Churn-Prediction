##Oversampld Data-----

write.csv(data, file = "C:\\Users\\admin\\Downloads\\hot_encoded_data.csv", row.names = FALSE)
DATA=read.csv("C:\\Users\\admin\\Downloads\\hot_encoded_data.csv") 
View(DATA)
install.packages("ROSE")
library(ROSE)

oversampled_data=ovun.sample(Churn~., data = DATA, method = "over", N = 1.5*10127, seed = 123)
oversampled_data=oversampled_data$data
dim(oversampled_data)


table(oversampled_data$Churn)


table(DATA$Churn)

Churn_percent_unbalanced=(1627/(1627+8500))*100;Churn_percent_unbalanced
Churn_percent_oversample=(6690/(8500+6690))*100;Churn_percent_oversample

##KNN ------
library(readxl)
library(class)
library(caret)
data=t(pca_df)
View(data)
n=nrow(data);n
dim(data)


n=nrow(DATA)
set.seed(100000)
train_index=sample(1:n,n*0.8,replace = FALSE)
train_data=DATA[train_index,]
test_data=DATA[-train_index,]
k=1:20
accc=F1score=c()
for(i in k)
  {
  predict_Churn=knn(train_data[,-1],test_data[,-1],train_data$Churn,k=i)
  c1=confusionMatrix(predict_Churn,as.factor(test_data$Churn))
  
  TP=c1$table[1,1];TP
  FP=c1$table[2,1];FP
  FN=c1$table[1,2];FN
  TN=c1$table[2,2];TN
  prec=TP/(TP+FP);prec
  reca=TP/(TP+FN);reca
  
  F1score[i]=2*(prec*reca)/(prec+reca)
  accc[i]=c1$overall[1]
}
F1score
accc

d=data.frame(k,F1score,accc);d
w=which(d$F1score==max(F1score));w
w1=which(d$accc==max(accc));w1
k=w

plot(k,accc)

### Multiple Samples on unbalanced data
SS=c(500, 1000, 1500, 2000, 3000,5000,10000)

RN=7

SENS=ACC=matrix(nrow=RN, ncol=RN)
n=nrow(DATA);n
for(i in 1:length(SS))
{
 index=replicate(n=RN,sample(1:n,SS[i],replace = FALSE))
  for(j in 1:RN)
  {
    train_index=sample(index[,j],0.8*SS[i],replace = FALSE)
    train_data=DATA[train_index,]
    test_data=DATA[-train_index,]
    predict_Churn=knn(train_data[,-1],test_data[,-1],train_data$Churn,k=6)
    cm1=confusionMatrix(predict_Churn,as.factor(test_data$Churn))
    ACC[j,i]=cm1$overall[1]
    SENS[j,i]=cm1$byClass[1]
  }
 
}
ACC
SENS

### Multiple Samples on unbalanced data
SS=c(500, 1000, 1500, 2000, 3000,5000,15000)

RN=7

SENS=ACC=matrix(nrow=RN, ncol=RN)
n=nrow(oversampled_data);n
for(i in 1:length(SS))
{
  index=replicate(n=RN,sample(1:n,SS[i],replace = FALSE))
  for(j in 1:RN)
  {
    train_index=sample(index[,j],0.8*SS[i],replace = FALSE)
    train_data=oversampled_data[train_index,]
    test_data=oversampled_data[-train_index,]
    predict_Churn=knn(train_data[,-1],test_data[,-1],train_data$Churn,k=12)
    cm1=confusionMatrix(predict_Churn,as.factor(test_data$Churn))
    ACC[j,i]=cm1$overall[1]
    SENS[j,i]=cm1$byClass[1]
  }
  
}
ACC
SENS

install.packages('DMwR')
library(caret)
library(ROSE)
library(caret)
library(DMwR)
# For oversampling

# Load your dataset (replace 'your_dataset.csv' with your dataset file)
data = DATA

# Define the number of folds for cross-validation
num_folds <- 5

# Create a K-fold cross-validation object
cv <- createFolds(data$Churn, k = num_folds, list = TRUE, returnTrain = FALSE)

# Define a function to perform oversampling and train the model for each fold
train_model <- function(train_index, test_index) {
  # Split the data into training and test sets based on fold indices
  train_data <- data[train_index, ]
  test_data <- data[test_index, ]
  
  # Perform oversampling on the training data
  train_data_oversampled <- ovun.sample(Churn ~ ., data = train_data, method = "over", N = nrow(train_data), seed = 123)$data
  
  # Train your model (replace this with your own modeling code)
  model <- train(Churn ~ ., data = train_data_oversampled, method = "over")
  
  # Evaluate the model on the test set
  predictions <- predict(model, newdata = test_data)
  
  # Calculate performance metrics (e.g., accuracy)
  accuracy <- confusionMatrix(predictions, test_data$target_variable)$overall["Accuracy"]
  
  return(accuracy)
}

# Initialize a vector to store the accuracy of each fold
accuracy_results <- numeric(num_folds)

# Perform K-fold cross-validation with oversampling
for (fold in 1:num_folds) {
  train_index <- unlist(cv[[fold]])
  test_index <- setdif
  
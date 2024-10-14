library(readxl)
library(caret)
library(class)
library(MASS)
library(e1071)
library(randomForest)

#Loading the data set

dim(D.training)
#Selecting the columns to be encoded
cols_to_encode=c("Gender", "Education_Level", "Marital_Status", "Income_Category", "Card_Category")

#Performing one-hot encoding
one_hot_encoded=dummyVars(" ~ .", data = d[, cols_to_encode])
one_hot_df=predict(one_hot_encoded, newdata = d)


#Optionally convert to matrix
one_hot_matrix=as.matrix(one_hot_df)
one_hot_df


#Extracting numerical columns from the original data frame
numerical_cols=c("Churn", "Customer_Age", "Dependent_count", "Months_on_book", 
                    "Total_Relationship_Count", "Months_Inactive_12_mon", "Contacts_Count_12_mon",
                    "Credit_Limit", "Total_Revolving_Bal", "Avg_Open_To_Buy", "Total_Amt_Chng_Q4_Q1",
                    "Total_Trans_Amt", "Total_Trans_Ct", "Total_Ct_Chng_Q4_Q1", "Avg_Utilization_Ratio")

#Appending numerical values to the one-hot encoded data frame
one_hot_df_with_numerical=cbind(d[numerical_cols],one_hot_df)

#Appending numerical values to the one-hot encoded matrix
one_hot_matrix_with_numerical <- cbind(one_hot_matrix, as.matrix(d[numerical_cols]))
View(one_hot_matrix_with_numerical)


#Logistic Regression
library(readxl)
library(caret)
d=read_excel("C:\\Users\\admin\\Downloads\\BankChurn_final.xlsx")
d$Avg_Open_To_Buy=as.numeric(as.factor(d$Avg_Open_To_Buy))
d$Gender=as.numeric(as.factor(d$Gender))
d$Education_Level=as.numeric(as.factor(d$Education_Level))
d$Marital_Status=as.numeric(as.factor(d$Marital_Status))
d$Income_Category=as.numeric(as.factor(d$Income_Category))
d$Card_Category=as.numeric(as.factor(d$Card_Category))
View(D)
str(d)
D=d[,-1]
dim(D)
w=sample(1:10127,10127*0.8,replace = F)
D.training=D[w,]
D.testing=D[-w,]
logistic.model=glm(formula='Churn~.',family=binomial,data=D.training)
summary(logistic.model)
PRE=predict(logistic.model,newdata= D.testing,type="response")
c1=confusionMatrix(as.factor(ifelse(PRE>0.5,1,0)),as.factor(D.testing$Churn));c1 

TP=c1$table[1,1];TP
FP=c1$table[2,1];FP
FN=c1$table[1,2];FN
TN=c1$table[2,2];TN

prec=TP/(TP+FP);prec
reca=TP/(TP+FN);reca
F1score=2*(prec*reca)/(prec+reca);F1score


#Naive Bayese
data=one_hot_df_with_numerical
View(data)
str(data)
n=nrow(data);n
set.seed(100)
train_index=sample(1:n,0.8*n,replace = F)
train_data=data[train_index,]
test_data=data[-train_index,]
model=naiveBayes(train_data[,-26],train_data[,26]);model
pre=predict(model,test_data[,-26]);pre
cm_nb=confusionMatrix(as.factor(test_data$Churn),pre)
cm_nb

#To calculate metrics
TP=cm_nb$table[2, 2]
TN=cm_nb$table[1, 1]
FP=cm_nb$table[1, 2]
FN=cm_nb$table[2, 1]

accuracy=sum(diag(cm_nb$table)) / sum(cm_nb$table)
precision=TP / (TP + FP)
recall=TP / (TP + FN)
f1_score=2 * (precision * recall) / (precision + recall)

metrics_table=data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score))

print(metrics_table)




#KNN

library(readxl)
library(class)
library(caret)
data=one_hot_df_with_numerical
n=nrow(data);n
set.seed(100000)
train_index=sample(1:n,0.8*n)
train_data=data[train_index,]
test_data=data[-train_index,]
k=3
predict_Churn=knn(train_data[,-26],test_data[,-26],train_data$Churn,k)
cm_knn=confusionMatrix(predict_Churn,as.factor(test_data$Churn))
cm_knn

#To calculate metrics
TP=cm_knn$table[2, 2]
TN=cm_knn$table[1, 1]
FP=cm_knn$table[1, 2]
FN=cm_knn$table[2, 1]

accuracy=sum(diag(cm_knn$table)) / sum(cm_knn$table)
precision=TP / (TP + FP)
recall=TP / (TP + FN)
f1_score=2 * (precision * recall) / (precision + recall)

metrics_table=data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score))

print(metrics_table)




#SVM

library(e1071)
n=nrow(data)

set.seed(100)
train_index=sample(1:n,0.8*n)
train_data=data[train_index,]
test_data=data[-train_index,]
model=svm(Churn~.,data=train_data,kernel="linear")
model
pre=predict(model,test_data)
cm_svm=confusionMatrix(as.factor(test_data$Churn),as.factor(ifelse(pre>median(pre),1,0)))
cm_svm

#To calculate metrics
TP=cm_svm$table[2, 2]
TN=cm_svm$table[1, 1]
FP=cm_svm$table[1, 2]
FN=cm_svm$table[2, 1]

accuracy=sum(diag(cm$table)) / sum(cm$table)
precision=TP / (TP + FP)
recall=TP / (TP + FN)
f1_score=2 * (precision * recall) / (precision + recall)

metrics_table=data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score))

print(metrics_table)




#Decision Trees
library(rpart)
library(rpart.plot)

set.seed(100) 
train_index=sample(1:nrow(data), 0.8 * nrow(data))
train_data=data[train_index, ]
test_data=data[-train_index, ]

tree_model=rpart(Churn ~ ., data = train_data, method = "class")

rpart.plot(tree_model, main = "Decision Tree")

predictions=predict(tree_model, test_data, type = "class")
cm_dt=confusionMatrix(predictions,as.factor(test_data$Churn))
cm_dt
# To calculate metrics
TP <- cm_dt$table[2, 2]
TN <- cm_dt$table[1, 1]
FP <- cm_dt$table[1, 2]
FN <- cm_dt$table[2, 1]

accuracy <- sum(diag(cm_dt$table)) / sum(cm_dt$table)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

metrics_table <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score))

print(metrics_table)




#Random Forest

data=one_hot_df_with_numerical
set.seed(100)
train_index=createDataPartition(data$Churn, p = 0.8, list = FALSE, times = 1)
train_data=data[train_index, ]
test_data=data[-train_index, ]
model=randomForest(Churn ~ ., data = train_data, proximity = TRUE)
predictions=predict(model, newdata = test_data)
cm_rf=confusionMatrix(as.factor(ifelse(predictions>median(predictions),1,0)), as.factor(test_data$Churn))
cm_rf

# To calculate metrics
TP=cm_rf$table[2, 2]
TN=cm_rf$table[1, 1]
FP=cm_rf$table[1, 2]
FN=cm_rf$table[2, 1]

accuracy=sum(diag(cm_rf$table)) / sum(cm_rf$table)
precision=TP / (TP + FP)
recall=TP / (TP + FN)
f1_score=2 * (precision * recall) / (precision + recall)

metrics_table=data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score))

print(metrics_table)


# Assuming you have already loaded your data into a data frame called one_hot_df_with_numerical

# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)

# Split data into predictors (X) and target variable (y)
X <- one_hot_df_with_numerical[, -target_column_index]  # Exclude the target column
y <- one_hot_df_with_numerical[, target_column_index]   # Target column index depends on your data

# Train a random forest model
rf_model <- randomForest(X, y)

# Get feature importance
importance <- importance(rf_model)

# Print feature importance
print(importance)
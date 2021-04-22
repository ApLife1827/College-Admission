                          #Project 2: Collage Admission
#Library
library(readxl)
library(plyr)
library(e1071)
library(rpart)
library(randomForest)
library(dplyr)
library(ggplot2)

#Path of Dataset
setwd("F:/PROGRAMMING/SimpliLearn/R for Data science/Project/Projects for Submission/Education")
getwd()

#Loading Dataset
data<-read.csv("Project 1_Dataset.csv")
View(data)

# Data sanity check
str(data)

#Checking Null Values in Dataset
is.null(data)

## Descriptive analysis
summary(data)
str(data)

boxplot(data$gre)
quantile(data$gre, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
data<-data[data$gre>=350,]

boxplot(data$gpa)
quantile(data$gpa, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
data<-data[data$gpa>=2.5,]


# Logistic Regression on full data----70.10% Accuracy
logistic<-glm(admit~.,data = data,family=binomial())
logistic
summary(logistic)

#Result: In summary I found Rank of institution is most significant for the student 
#admission. With that GPA and GRE are also the significant variable. 
#Socioeconomic status, Gender and Race are not significant variable.


# Predicted Probabilities
result<-predict(logistic,data)
result
summary(result)
res<-ifelse(result > 0, 1, 0)

#Accuracy of the model
accuracy <- table(res, data[,1])
sum(diag(accuracy))/sum(accuracy)

#Droping the insignificant variable
data<-data[,-c(4,5,6)]
data1<-data


# Converting necessary variables into factor
data$admit <- as.factor(data$admit)
data$admit

#To decide the which model more sutaible for Dataset I compare Naive bayes, SVM, Decision Tree and Random Forest
#1. Naive Bayes----------69.58% Accuracy
naive_bayes<-naiveBayes(admit~.,data = data)
summary(naive_bayes)

Predictions<-predict(naive_bayes,data)
Predictions
table(Predictions,data$admit)

#Accuracy of Model
table_mat<-table(Predictions,data$admit)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#2. Support Vector Machine-------72.42% Accuracy
svms<-svm(admit~.,data = data,method = 'class')
summary(svms)

Predictions<-predict(svms,data,type='class')
Predictions
table(Predictions,data$admit)

#Accuracy of Model
table_mat<-table(Predictions,data$admit)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#3. Decision Tree---------74.74% Acuraccy
tree<-rpart(admit~.,data = data,method = 'class')
summary(tree)

Predictions<-predict(tree,data,type = 'class')
Predictions
table(Predictions,data$admit)

#Accuracy of Model
table_mat<-table(Predictions,data$admit)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#4. Random Forest---------100% Accuracy
forest<-randomForest(x = data, y = data$admit,ntree =800)# build model
summary(forest)

Predictions<-predict(forest,data)
Predictions
table(Predictions,data$admit)

#Accuracy of Model
table_mat<-table(Predictions,data$admit)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#Step 1 - Categorize the grade point average into High or Medium and plot it on point chart.
Aptitute_Descriptive = transform(data1,
                                 GreLevels=ifelse(gre<439,"Low",ifelse(gre<579,"Medium","High")))
str(Aptitute_Descriptive)
Sum_Apt=aggregate(admit~GreLevels,data = Aptitute_Descriptive,FUN=sum)
length_Apt=aggregate(admit~GreLevels,Aptitute_Descriptive,FUN=length)
Probability_Table = cbind(Sum_Apt,Recs=length_Apt[,2])
Probability_Table_final = transform(Probability_Table,Probability_Admission =
                                      admit/Recs)
Probability_Table_final
ggplot(Probability_Table_final,aes(x=GreLevels,y=Probability_Admission))+geom_point()

#Step 2: Cross grid for admission variable with GRE categorized

table(Aptitute_Descriptive$admit,Aptitute_Descriptive$GreLevels)

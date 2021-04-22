# College-Admission
## Problem Statement:
An education department in the US needs to analyze the factors that influence the
admission of a student into a college. Analyze the historical data and determine the
key drivers.
Analysis information:
Predictive:
 - Run logistic model to determine the factors that influence the admission process
   of a student (Drop insignificant variables)
 - Transform variables to factors wherever required
 - Calculate accuracy of the model
 - Try other modeling techniques like decision tree and SVM and select a champion
   model
 - Determine the accuracy rates for each model
 - Select the most accurate model
 - Identify other Machine learning or statistical techniques that can be used
  
 Descriptive:
 - Categorize the grade point average into High, Medium, and Low (with admission
   probability percentages) and plot it on a point chart

## Descriptive analysis
     summary(data)
     str(data)
     boxplot(data$gre)     
     quantile(data$gre, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
     data<-data[data$gre>=350,]
     boxplot(data$gpa)
     quantile(data$gpa, c(0,0.05,0.1,0.25,0.5,0.75,0.90,0.95,0.97,0.98,0.985,0.99,0.995,1))
     data<-data[data$gpa>=2.5,]

## Logistic Regression:
     logistic<-glm(admit~.,data = data,family=binomial())
     summary(logistic)
### Predicted Probabilities
    result<-predict(logistic,data)
    summary(result)
    res<-ifelse(result > 0, 1, 0)
### Accuracy of the model
    accuracy <- table(res, data[,1])
    sum(diag(accuracy))/sum(accuracy)

## Random Forest
    forest<-randomForest(x = data, y = data$admit,ntree =800)
    summary(forest)
    Predictions<-predict(forest,data)
    table(Predictions,data$admit)
### Accuracy of Model
    table_mat<-table(Predictions,data$admit)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test

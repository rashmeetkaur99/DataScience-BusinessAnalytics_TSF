#TASK-1

# Prediction using supervised learning
# Simple Linear Regression
# Importing the dataset
library(readxl)
dataset = read_excel("C:\\Users\\lenovo\\Desktop\\internship-task\\regression.xlsx")
View(dataset)
nrow(dataset)
dataset=as.data.frame(dataset)
class(dataset)
#loading library

library(rpart)
library(dplyr)
library(caTools)

#creating train and test sets
plot(dataset)
split_values<-sample.split(dataset$Scores,SplitRatio =0.9)

train_sample<-subset(dataset,split_values==T)
nrow(train_sample)
test_sample<-subset(dataset,split_values==F)
nrow(test_sample)
# creating the apppropriate linear model
plot(dataset)
y=dataset$Scores
x=dataset$Hours
model=lm(y~x,train_sample)
print(summary(model))

result=predict(object = model ,newdata =  test_sample)
result

z=data.frame(pred=result,actual=dataset$Scores)
z
mse=sqrt(mean((z$actual-z$pred)^2))
mse

a=data.frame(x=9.25)
final_result=predict(model, a)
final_result

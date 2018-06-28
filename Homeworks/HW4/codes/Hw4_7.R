setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW4")
library(ISLR)
data("OJ")
OJ = data.frame(OJ)

sum(is.na(OJ))
summary(OJ)
str(OJ)
#changing the type of the features. 

OJ$StoreID = as.factor(as.character(OJ$StoreID))
OJ$SpecialCH = as.factor(as.character(OJ$SpecialCH))
OJ$SpecialMM = as.factor(as.character(OJ$SpecialMM))
OJ$STORE = as.factor(as.character(OJ$STORE))

#divide train and test. 
set.seed(1234)
train_index = sample(1:nrow(OJ), 0.7*nrow(OJ))
train = OJ[train_index,]
test = OJ[-train_index,]


#fit models. 
#default svm
#####
library(e1071)
library(Metrics)
train_errors = c()
test_errors = c()
cost_list = seq(0.01, 10, 0.1)
cost_list = append(cost_list, 10)
count = 0
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i)
  predictions = predict(svm_model, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  test_errors = append(test_errors, test_error)
  
  predictions = predict(svm_model, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  train_errors = append(train_errors, train_error)
  count = count + 1
  print(paste(count, "of", length(cost_list)))
}


plot_results = data.frame(cost_list, train_errors, test_errors)
names(plot_results) = c("cost", "train_error", "test_error")

library(plotly)
p = plot_ly(plot_results, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Train Vs Test Error Performance",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p
#####


#radial kernel
#####
train_errors = c()
test_errors = c()
cost_list = seq(0.01, 10, 0.1)
cost_list = append(cost_list, 10)
count = 0
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i, kernel = "radial")
  predictions = predict(svm_model, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  test_errors = append(test_errors, test_error)
  
  predictions = predict(svm_model, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  train_errors = append(train_errors, train_error)
  count = count + 1
  print(paste(count, "of", length(cost_list)))
}


plot_results = data.frame(cost_list, train_errors, test_errors)
names(plot_results) = c("cost", "train_error", "test_error")

library(plotly)
p = plot_ly(plot_results, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Train Vs Test Error Performance. Radial Kernel.",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p
#####


#polynomial kernel. degree = 2
#####
train_errors = c()
test_errors = c()
cost_list = seq(0.01, 10, 0.1)
cost_list = append(cost_list, 10)
count = 0
for(i in cost_list){
  svm_model = svm(Purchase~., train, cost = i, kernel = "polynomial", degree = 2)
  predictions = predict(svm_model, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  test_errors = append(test_errors, test_error)
  
  predictions = predict(svm_model, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  train_errors = append(train_errors, train_error)
  count = count + 1
  print(paste(count, "of", length(cost_list)))
}


plot_results = data.frame(cost_list, train_errors, test_errors)
names(plot_results) = c("cost", "train_error", "test_error")

library(plotly)
p = plot_ly(plot_results, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Train Vs Test Error Performance. Polynomial Kernel.",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p
#####

#####
#Q3
#####
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW1")

library(ElemStatLearn)
data("zip.train")
data("zip.test")
zip.train = data.frame(zip.train)
zip.test = data.frame(zip.test)

#Let's take out 2,3 from train and test as targets.
train = zip.train[which(zip.train$X1 == 2),]; train = rbind(train, zip.train[which(zip.train$X1 == 3),]); rm(zip.train)
test = zip.test[which(zip.test$X1 == 2),]; test = rbind(test, zip.test[which(zip.test$X1 == 3),]); rm(zip.test)

#convert the 2s to 0 and 3s to 1.
train$X1 = sapply(train$X1, function(x) ifelse(x == 2, 0, 1))
test$X1 = sapply(test$X1, function(x) ifelse(x == 2, 0, 1))


#Convert the target column into categorical feature. 
train$X1 = as.factor(as.character(train$X1))
test$X1 = as.factor(as.character(test$X1))
table(train$X1)
#0   1 
#731 658 

#Method for RSS
get_RSS = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + (Truevalues[i] - Predictedvalues[i])^2
    #print(sum)
  }
  return(sum)
}

#Decide 1 or 0 from predictions
decide_Class = function(x){
  if(x > 0){
    distance_to0 = x
    distance_to1 = abs(x - 1)
  }else{
    return(0)
  }
  class = ifelse(distance_to0 < distance_to1, 0, 1)
  return(class)
}


#Let's create lists for us to capture error and method/k value for plotting later. 
k_list = c() #This also has the regression.
train_error = c()
test_error = c()
train_targets = as.numeric(as.character(train$X1))
test_targets = as.numeric(as.character(test$X1))

#Run Regression. 
k_list = append(k_list, "Regression")
regression_model = lm(X1~., train)
train_predictions = predict(regression_model, train[,2:ncol(train)]) 
train_predictions = sapply(train_predictions, decide_Class)
train_error = append(train_error, get_RSS(train_targets, train_predictions))

test_predictions = predict(regression_model, test[,2:ncol(test)])
test_predictions = sapply(test_predictions, decide_Class)
test_error = append(test_error, get_RSS(test_targets, test_predictions))

#KNN classification
library(class)
for(i in c(1,3,5,7,9,11,13,15)){
  print(paste("k:", i, sep = " "))
  k_list = append(k_list, i)
  knn_train_predictions = knn(train = train[,2:ncol(train)], test = train[,2:ncol(train)], 
                        cl = train$X1, k = i)
  knn_train_predictions = as.numeric(as.character(knn_train_predictions))
  
  knn_test_predictions = knn(train = train[,2:ncol(train)], test = test[,2:ncol(train)], 
                         cl = train$X1, k = i)
  knn_test_predictions = as.numeric(as.character(knn_test_predictions))
  
  train_error = append(train_error, get_RSS(train_targets, knn_train_predictions))
  test_error = append(test_error, get_RSS(test_targets, knn_test_predictions))
}

results = data.frame(k_list, train_error, test_error)
rm(train_error, test_error)

library(reshape2)
library(ggplot2)
plot_data = melt(results, id = "k_list")
plot_data$k_list = factor(plot_data$k_list, levels = plot_data$k_list[1:9])
ggplot(data = plot_data, aes(x = k_list, y = value, colour = variable)) + geom_point() +
  labs(title = "TrainError and TestError comparision")

#The error by Regression is so much that we cannot see the KNN results clearly. 
#Let's remove regression and then plot to see the performance of KNN. 

plot_data = melt(results[2:nrow(results),], id = "k_list")
plot_data$k_list = factor(plot_data$k_list, levels = plot_data$k_list[1:8])
ggplot(data = plot_data, aes(x = k_list, y = value, colour = variable)) + geom_point() +
  labs(title = "TrainError and TestError comparision without Regression")


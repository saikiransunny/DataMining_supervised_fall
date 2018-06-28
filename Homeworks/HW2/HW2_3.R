########################
#Clean the work station.
########################
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW2")
######
#Q3
######

##################################################################################
#Generate 20 cols with 1000 numbers. 
#Most of the algorithms assume the data to be in gaussian distribution, hence
#Generating the numbers in gaussian distribution. 
##################################################################################
#######
set.seed(1234)
dataset = rnorm(1000)
for(i in 1:19){
  dataset = cbind(dataset, rnorm(1000))
}
dataset = data.frame(dataset)
names(dataset)[1] = "V1"
######



#Generate target. 
######
#Genrating random coefficients. 
coeff = rexp(21)
#Let's nullify 2 coefficients. 
zero_index = sample(1:20, 2)
coeff[zero_index] = 0

#Get Y
getY = function(row){
  sum = 0
  for(i in 1:length(row)){
    sum = sum + row[i]*coeff[i]
  }
  sum = sum + coeff[21]
  return(sum)
}
######
dataset$Y = apply(dataset, MARGIN = 1, getY)




#Generate train and test datasets. 
train.index = sample(1:nrow(dataset), 100)
train_dataset = dataset[train.index,]
test_dataset = dataset[-train.index,]


#store the error value and the methods. 
method = c()
error = c()

lm_model = lm(Y~., train_dataset)
lm_predictions = predict(lm_model, test_dataset[,-c(21)])

get_MeanSquareError = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + ((Truevalues[i] - Predictedvalues[i])^2)
  }
  sum = sum / length(Truevalues)
  return(sum)
}
result = get_MeanSquareError(Truevalues = test_dataset$Y, Predictedvalues = lm_predictions)
method = append(method, "regression")
error = append(error, result)




##################
#
#ridge model. 
##################
library(glmnet)
ridge_model = cv.glmnet(x = as.matrix(train_dataset[,-c(21)]), y = train_dataset$Y, alpha = 0)
summary(ridge_model)
plot(ridge_model)
best_lambda = ridge_model$lambda.min
ridge_predictions = predict(ridge_model, s = best_lambda, newx = as.matrix(test_dataset[,-c(21)]),
                            type =  "response")
result = get_MeanSquareError(Truevalues = test_dataset$Y, ridge_predictions)
method = append(method, "Ridge")
error = append(error, result)





###############
##
##lasso model. 
###############
lasso_model = cv.glmnet(x = as.matrix(train_dataset[,-c(21)]), y = train_dataset$Y, alpha = 1)
summary(lasso_model)
plot(lasso_model)
best_lambda = lasso_model$lambda.min
lasso_predictions = predict(lasso_model, s = best_lambda, newx = as.matrix(test_dataset[,-c(21)]), 
                            type = "response")
result = get_MeanSquareError(Truevalues = test_dataset$Y, lasso_predictions)
method = append(method, "Lasso")
error = append(error, result)






################
#
#forward model
################
library(leaps)
get_feature_index = function(small_list, complete_list){
  index = c()
  for(feature_index_i in small_list){
    found_at = grep(pattern = paste("\\b", feature_index_i, "\\b", sep=""),
                    x = complete_list)
    index = append(index, found_at)
    #print(paste(feature_index_i, ": ", found_at))
  }
  return(unique(index))
}
forward_model = regsubsets(Y~., nvmax = 20, method = "forward", data = train_dataset)


library(ggplot2)
#predicting process in forward model. 
new_train = cbind(rep(1, nrow(train_dataset)), train_dataset) 
colnames(new_train) = c("Intercept", names(train_dataset))
new_test = cbind(rep(1, nrow(test_dataset)), test_dataset)
colnames(new_test) = c("Intercept", names(test_dataset))
train_forward_error = c()
test_forward_error = c()
for(i in 1:20){
  print(i)
  coef_i = coef(forward_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  
  
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  
  train_forward_error = append(train_forward_error,
                               get_MeanSquareError(train_dataset$Y, train_preds))
  test_forward_error = append(test_forward_error, 
                              get_MeanSquareError(test_dataset$Y, test_preds))
}
plot_data = data.frame(train_forward_error, test_forward_error)
plot_data$n = rownames(plot_data)

plot_data$n = factor(plot_data$n, levels = plot_data$n)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_forward_error, color = "train_forward_error")) + 
  geom_line(aes(y = test_forward_error, color = "test_forward_error")) + ggtitle("Train and Test error comparison: Forward") +
  ylab("Error")  
method = append(method, "Forward")
error = append(error, min(test_forward_error))




###########################
#
#backward model
##############################
backward_model = regsubsets(Y~., nvmax = 20, method = "backward", data = train_dataset)

#predicting process in backward model. 
new_train = cbind(rep(1, nrow(train_dataset)), train_dataset) 
colnames(new_train) = c("Intercept", names(train_dataset))
new_test = cbind(rep(1, nrow(test_dataset)), test_dataset)
colnames(new_test) = c("Intercept", names(test_dataset))
train_backward_error = c()
test_backward_error = c()
for(i in 1:20){
  print(i)
  coef_i = coef(backward_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  
  
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  
  train_backward_error = append(train_backward_error,
                                get_MeanSquareError(train_dataset$Y, train_preds))
  test_backward_error = append(test_backward_error, 
                               get_MeanSquareError(test_dataset$Y, test_preds))
}
plot_data = data.frame(train_backward_error, test_backward_error)
plot_data$n = rownames(plot_data)

library(ggplot2)
plot_data$n = factor(plot_data$n, levels = plot_data$n)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_backward_error, color = "train_backward_error")) + 
  geom_line(aes(y = test_backward_error, color = "test_backward_error")) + ggtitle("Train Test comparison: Backward")+
  ylab("Error")  


method = append(method, "Backward")
error = append(error, min(test_backward_error))





#####################
#
#exhaustive model
#####################
exhaustive_model = regsubsets(Y~., nvmax = 20, method = "exhaustive", data = train_dataset, really.big = T)
#predicting process in exhaustive model. 
new_train = cbind(rep(1, nrow(train_dataset)), train_dataset) 
colnames(new_train) = c("Intercept", names(train_dataset))
new_test = cbind(rep(1, nrow(test_dataset)), test_dataset)
colnames(new_test) = c("Intercept", names(test_dataset))
train_exhaustive_error = c()
test_exhaustive_error = c()
for(i in 1:20){
  print(i)
  coef_i = coef(exhaustive_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  
  
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  
  train_exhaustive_error = append(train_exhaustive_error,
                                get_MeanSquareError(train_dataset$Y, train_preds))
  test_exhaustive_error = append(test_exhaustive_error, 
                               get_MeanSquareError(test_dataset$Y, test_preds))
}
plot_data = data.frame(train_exhaustive_error, test_exhaustive_error)
plot_data$n = rownames(plot_data)

library(ggplot2)
plot_data$n = factor(plot_data$n, levels = plot_data$n)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_exhaustive_error, color = "train_exhaustive_error")) + 
  geom_line(aes(y = test_backward_error, color = "test_exhaustive_error")) + ggtitle("Train Test comparison: Exhaustive")+
  ylab("Error")  


method = append(method, "Exhaustive")
error = append(error, min(test_exhaustive_error))








###############
#
#plot results. 
###############
plot_data = data.frame(method, error)
plot_data$method = factor(plot_data$method, levels = plot_data$method)
#Different methods comparison. 
g = ggplot(plot_data, aes(method, error))
g + geom_bar(stat = "identity") + ggtitle("Method comparison")

#Different methods comparison without ridge and lasso. 
plot_data = plot_data[-c(2,3),]
g = ggplot(plot_data, aes(method, error))
g + geom_bar(stat = "identity") + ggtitle("Method comparison without Ridge and Lasso")

cor(coeff, lm_model$coefficients[2:21])

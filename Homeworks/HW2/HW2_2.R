########################
#Clean the work station.
########################
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW2")
######
#Q2
######

train_data = read.table("training_ticdata2000.txt")
head(train_data)
summary(train_data)
names(train_data)
sum(is.na(train_data))


#There probaby will be outliers in the data. But checking for all the 86 features is a tedious tak. 
#Let's try to automate it. 

plot_function = function(feature, featurename){
  par(mfrow=c(1,2)) 
  plot(density(feature), main = paste("Density plot of ", featurename, sep = ""))
  boxplot(feature, main = paste("Boxplot of ", featurename, sep = ""))
  
  
  dev.copy(png,paste(getwd(), "/plots/Q2/density_plots/",featurename, ".png", sep = ""),
           width=8,height=6,units="in",res=100)
  dev.off()
}
for(i in 1:85){
  plot_function(train_data[,i], names(train_data)[i])
  
  print(table(train_data[,i]))
}
#Looking at the table data. It is difficult to estimate which one are the probable score and which are
#the categorical variables. 
#Hence leaving the dataset as it is, without changing the field type. 

#Removing all the data points that are indicated by the boxplot as outliers, is taking off too many datapoints. 
#Let's remove the outliers only if there are <10 outliers per feature. 

temp_train = train_data
for(i in 1:85){
  rm.index = c()
  outliers = unique(sort(boxplot.stats(temp_train[,i])$out))
  if(length(outliers) > 0){
    if(length(outliers) <= 10){
    outlier_index = c()
    for(outlier in 1:length(outliers)){
      outlier_index = append(outlier_index, grep(outliers[outlier], temp_train[,i]))
    }
    outlier_index = unique(outlier_index)
    rm.index = append(rm.index, outlier_index)
    
    rm.index = unique(rm.index)
    temp_train = temp_train[-rm.index,]
  }
}
}
#As it can be seen though we are giving restrictions to the number of outliers that should be present 
#for outliers to be removed, too many data points are being removed. 
#Hence let us omit the outlier removal. 
rm(temp_train)


#load the test data. 
test_data = read.table("testing_ticeval2000.txt")
test_Y = read.table("testtargets_tictgts2000.txt")
test_data$V86 = test_Y$V1

#store the error value and the methods. 
method = c()
error = c()

lm_model = lm(V86~., train_data)
lm_predictions = predict(lm_model, test_data)

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
lm_predictions = sapply(lm_predictions, decide_Class)

table(lm_predictions)
#The model seems to be doing a very bad job at predicting. Let's check the error. 
#Least squares 
get_LeastSquares = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + ((Truevalues[i] - Predictedvalues[i])^2)
  }
  #print(sum)
  return(sum)
}
result = get_LeastSquares(Truevalues = test_Y$V1, Predictedvalues = as.numeric(lm_predictions))
table(test_Y)
method = append(method, "regression")
error = append(error, result)
#Though 239 of test error seems low, the fact that there are only 238 1s in the test set indicates that
#the model is performing poorly. 
#Predicting all the data points as 0 will fetch us 238 as error. Hence we are doing very bad currently!

library(glmnet)
ridge_model = cv.glmnet(x = as.matrix(train_data[,-c(86)]), y = train_data$V86, alpha = 0)
summary(ridge_model)
plot(ridge_model)
best_lambda = ridge_model$lambda.min
ridge_predictions = predict(ridge_model, s = best_lambda, newx = as.matrix(test_data[,c(1:85)]),
                            type =  "response")
ridge_predictions = sapply(ridge_predictions, decide_Class)
table(ridge_predictions)
result = get_LeastSquares(Truevalues = test_Y$V1, as.numeric(ridge_predictions))
method = append(method, "Ridge")
error = append(error, result)





###############
##
##lasso model. 
###############
lasso_model = cv.glmnet(x = as.matrix(train_data[,-c(86)]), y = train_data$V86, alpha = 1)
summary(lasso_model)
plot(lasso_model)
best_lambda = lasso_model$lambda.min
lasso_predictions = predict(lasso_model, s = best_lambda, newx = as.matrix(test_data[,c(1:85)]), 
                            type = "response")
lasso_predictions = sapply(lasso_predictions, decide_Class)
table(lasso_predictions)
result = get_LeastSquares(Truevalues = test_Y$V1, as.numeric(lasso_predictions))
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
forward_model = regsubsets(V86~., nvmax = 85, method = "forward", data = train_data)

library(ggplot2)
#predicting process in forward model. 
new_train = cbind(rep(1, nrow(train_data)), train_data) 
colnames(new_train) = c("Intercept", names(train_data))
new_test = cbind(rep(1, nrow(test_data)), test_data)
colnames(new_test) = c("Intercept", names(test_data))
train_forward_error = c()
test_forward_error = c()
for(i in 1:85){
  print(i)
  coef_i = coef(forward_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  train_preds = sapply(train_preds, function(x) decide_Class(x))
  
  
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  test_preds = sapply(test_preds, function(x) decide_Class(x))

  train_forward_error = append(train_forward_error,
                                get_LeastSquares(train_data$V86, train_preds))
  test_forward_error = append(test_forward_error, 
                              get_LeastSquares(test_data$V86, test_preds))
}
plot_data = data.frame(train_forward_error, test_forward_error)
plot_data$n = rownames(plot_data)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_forward_error, color = "train_forward_error")) + 
  geom_line(aes(y = test_forward_error, color = "test_forward_error")) + ggtitle("Train and Test error comparison: Forward")

method = append(method, "Forward")
error = append(error, min(test_forward_error))




###########################
#
#backward model
##############################
backward_model = regsubsets(V86~., nvmax = 85, method = "backward", data = train_data)

#predicting process in backward model. 
new_train = cbind(rep(1, nrow(train_data)), train_data) 
colnames(new_train) = c("Intercept", names(train_data))
new_test = cbind(rep(1, nrow(test_data)), test_data)
colnames(new_test) = c("Intercept", names(test_data))
train_backward_error = c()
test_backward_error = c()
for(i in 1:85){
  print(i)
  coef_i = coef(backward_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  train_preds = sapply(train_preds, function(x) decide_Class(x))
  
  
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  test_preds = sapply(test_preds, function(x) decide_Class(x))
  
  train_backward_error = append(train_backward_error,
                               get_LeastSquares(train_data$V86, train_preds))
  test_backward_error = append(test_backward_error, 
                              get_LeastSquares(test_data$V86, test_preds))
}
plot_data = data.frame(train_backward_error, test_backward_error)
plot_data$n = rownames(plot_data)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_backward_error, color = "train_backward_error")) + 
  geom_line(aes(y = test_backward_error, color = "test_backward_error")) + ggtitle("Train Test comparison: Backward")

method = append(method, "Backward")
error = append(error, min(test_backward_error))



plot_data = data.frame(method, error)
#Different methods comparison. 
g = ggplot(plot_data, aes(method, error))
g + geom_bar(stat = "identity") + ggtitle("Method comparison")


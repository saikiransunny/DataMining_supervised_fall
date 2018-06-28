rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW3/codes")

library(MASS)
data("Boston")
sum(is.na(Boston))

names(Boston)
summary(Boston)

Median_crime = median(Boston$crim)
Boston$crim = sapply(Boston$crim, function(x) ifelse(x <  Median_crime, 0 , 1))
Boston$crim = as.factor(as.character(Boston$crim))



#generate train and test datasets. 
train_index = sample(1:nrow(Boston), 0.7*nrow(Boston))
train = Boston[train_index,]
test = Boston[-train_index,]

#get leastsquares. 
get_LeastSquares = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + ((Truevalues[i] - Predictedvalues[i])^2)
  }
  #print(sum)
  return(sum)
}


#maintain holders for plotting later. 
logistic_results = c()
LDA_results = c()
KNN_results = c()

library(leaps)
library(class)
#This is only to select the subset of the features. 
forward_model = regsubsets(crim~., nvmax = 13, method = "forward", data = Boston)
for(i in 2:13){
  feature_names = names(coef(forward_model, i))[1:i+1]
  feature_names = append(feature_names, "crim")
  
  train_subset = subset(train, select = c(feature_names))
  test_subset = subset(test, select = c(feature_names))
  
  #lda model
  lda_model = lda(crim~., train_subset)
  lda_predictions = predict(lda_model, test_subset)$class
  LDA_results = append(LDA_results, get_LeastSquares(as.numeric(as.character(test_subset$crim)), 
                                                     as.numeric(as.character(lda_predictions))))
  
  #knn
  #Taking the best of k for given dataset. 
  best_knn = c()
  for(j in c(3,5,7,9)){
  knn_model_predictions = knn(train = train_subset[,1:ncol(train_subset)-1], 
                              test = test_subset[,1:ncol(test_subset)-1], cl = train_subset$crim, k = j)
  best_knn = append(best_knn, get_LeastSquares(as.numeric(as.character(test_subset$crim)), 
                                               as.numeric(as.character(knn_model_predictions))))
  }
  print(which(best_knn == min(best_knn)))
  KNN_results = append(KNN_results, min(best_knn))
  
  #logistic regression
  #convert the crim to numeric. 
  train_subset$crim = as.numeric(as.character(train_subset$crim))
  logistic_model = glm(crim~., data = train_subset)
  logistic_predictions = predict(logistic_model, test_subset[,1:ncol(test_subset)-1])
  logistic_predictions = sapply(logistic_predictions, function(x) ifelse(x < 0.5, 0, 1))
  logistic_results = append(logistic_results, get_LeastSquares(as.numeric(as.character(test_subset$crim)),
                                                               logistic_predictions))
}


#plotting. 
library(reshape2)
plot_data = data.frame(LDA_results,logistic_results,  KNN_results)
plot_data$no_attr = as.numeric(as.character(rownames(plot_data)))+1
plot_data$no_attr = factor(plot_data$no_attr, levels = plot_data$no_attr)
plot_data = melt(plot_data, id = "no_attr")
names(plot_data) = c("No_of_Attributes", "variable", "Error")

library(plotly)
p = plot_ly(data = plot_data, x = ~No_of_Attributes, y = ~Error, color = ~variable)
p

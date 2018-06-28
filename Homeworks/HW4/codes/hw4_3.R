#choosing the boston dataset. 

rm(list = ls(all = T))

library(MASS)
library(Metrics)
boston = data("Boston")
boston = data.frame(boston)

#sanity check
sum(is.na(Boston))

set.seed(1234)
train_index = sample(1:nrow(Boston), 0.8*nrow(Boston))
train = Boston[train_index,]
test = Boston[-train_index,]
rm(boston, train_index)

model = c()
RMSE = c()
#regression
linear_model = glm(crim~., train, family = "gaussian")
linear_predictions = predict(linear_model, test)
model = append(model, "Regression")
RMSE = append(RMSE, Metrics::rmse(test$crim, linear_predictions))


#knn
library(FNN)
knn_predictions = knn.reg(train = train[,-c(1)], test = test[,-c(1)], y = train$crim, k = 3)
model = append(model, "KNN")
RMSE = append(RMSE, Metrics::rmse(test$crim, knn_predictions$pred))

#random_forest
library(randomForest)
rf.model = randomForest(crim~., train, ntree = 100, mtry = 9, sampsize = 300)
rf.predictions = predict(rf.model, test)
model = append(model, "RF")
RMSE = append(RMSE,Metrics::rmse(test$crim, rf.predictions))


#boosting
library(xgboost)
xgboost_train = as.matrix(train[,-c(1)])
xgboost_test = as.matrix(test[,-c(1)])
xgboost_model = xgboost(xgboost_train, train$crim, nrounds = 50, eta = 0.1, max_depth = 4, objective = "reg:linear")
xgboost_predictions = predict(xgboost_model, xgboost_test)
model = append(model, "XGBoost")
RMSE = append(RMSE, Metrics::rmse(test$crim, xgboost_predictions))


#bagging on trees. 
library(rpart)
sampling = 300    # no of data points. 
iterations = 200  #no of bags. 
tree_RMSE = c()
for(i in seq(1,iterations)){
  new_train = train[sample(1:nrow(train), sampling),]
  tree_model = rpart(crim~., new_train)
  tree_predictions = predict(tree_model, test)
  tree_RMSE = append(tree_RMSE, Metrics::rmse(test$crim, tree_predictions))
}
mean_tree_RMSE = mean(tree_RMSE)
model = append(model, "Bagging_Tree")
RMSE = append(RMSE, mean_tree_RMSE)



plot_results = data.frame(model, RMSE)


library(plotly)
p = plot_ly(plot_results, x = ~model, y = ~RMSE, name = 'Model', type = 'scatter', mode = 'markers') %>%
  layout(title = "Models Vs RMSE",
         xaxis = list(title = "Models", type = "category"),
         yaxis = list (title = "RMSE"))
p

#results from RF and Xgboost are best. 
library(GGally)
ggpairs(Boston)

library(partykit)
fancyRpartPlot(tree_model)

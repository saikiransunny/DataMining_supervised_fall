rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/project")

train = read.csv("data/after_humanfeaturegen/train.csv")
test = read.csv("data/after_humanfeaturegen/test.csv")


train_model = subset(train, select = -c(Name, Publisher))
test_model = subset(test, select = -c(Name, Publisher))
rm(train, test)

str(train_model)
change_factors = function(dataframe){
  dataframe$yearI = as.factor(as.character(dataframe$yearI))
  dataframe$yearII = as.factor(as.character(dataframe$yearII))
  dataframe$is_important_genre = as.factor(as.character(dataframe$is_important_genre))
  dataframe$is_important_platform = as.factor(as.character(dataframe$is_important_platform))
  
  return(dataframe)
}
train_model = change_factors(train_model)
test_model = change_factors(test_model)
str(train_model)
str(test_model)

#prepare for standadization and onehot encoding. 
######
#train
library(mltools)
library(data.table)
library(vegan)
train_cat_index = c(1,2,6,13,14,15,16,17)
train_numeric = train_model[,-train_cat_index]
train_cat = train_model[,train_cat_index]
train_cat = data.frame(one_hot(data.table(train_cat)))
train_cat = lapply(train_cat, function(x) as.factor(as.character(x)))
train_Global_Sales = train_model$Global_Sales
train_numeric = subset(train_numeric, select = -c(Global_Sales))
test_Global_Sales = test_model$Global_Sales
train_numeric = decostand(train_numeric, method = "standardize")
std_train = train_numeric
std_train = cbind(std_train, train_cat)
std_train$Global_Sales = train_Global_Sales


#test
test_cat_index = c(1,2,6,13,14,15,16,17)
test_numeric = test_model[,-test_cat_index]
test_cat = test_model[,test_cat_index]
test_cat = data.frame(one_hot(data.table(test_cat)))
test_cat = lapply(test_cat, function(x) as.factor(as.character(x)))
test_Global_Sales = test_model$Global_Sales
test_numeric = subset(test_numeric, select = -c(Global_Sales))
test_Global_Sales = test_model$Global_Sales

test_numeric = decostand(test_numeric, method = "standardize")

std_test = test_numeric
std_test = cbind(std_test, test_cat)
std_test$Global_Sales = test_Global_Sales
######
rm(train_numeric, test_numeric, test_cat, train_cat)
library(Metrics)
#maintain different holders for methods and results. 
model_name = c()
error = c()

glm_model = glm(Global_Sales~., train_model, family = "gaussian")
summary(glm_model)
glm_predictions = predict(glm_model, test_model[,-c(3)])
model_name = append(model_name, "Regression")
#rmse(actual = test_model$Global_Sales, predicted = glm_predictions)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = glm_predictions))

#knn
std_train = data.frame(lapply(std_train, function(x) as.numeric(as.character(x))))
std_test = data.frame(lapply(std_test, function(x) as.numeric(as.character(x))))
library(FNN)
knn_pred = knn.reg(train = std_train[,-c(93)], test = std_test[,-c(93)], 
                              y = std_train$Global_Sales, k = 3)
#rmse(actual = std_test$Global_Sales, predicted = knn_pred$pred)
model_name = append(model_name, "KNN")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = knn_pred$pred))


library(e1071)
svm_model = svm(Global_Sales~., train_model)
svm_predictions = predict(svm_model, test_model[,-c(3)])
model_name = append(model_name, "SVM")
#rmse(actual = test_model$Global_Sales, predicted = svm_predictions)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = svm_predictions))

svm_model1 = svm(Global_Sales~., train_model, kernel = "linear")
svm_predictions1 = predict(svm_model1, test_model[,-c(3)])
model_name = append(model_name, "SVM_Linear")
#rmse(actual = test_model$Global_Sales, predicted = svm_predictions1)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = svm_predictions1))

# i = 0
# cos_list = c()
# gam_list = c()
# for(cos in seq(1,30,5)){
#   for(gam in seq(0,0.5,0.1)){
#     i = i + 1
# svm_model2 = svm(Global_Sales~., train_model, kernel = "radial", gamma = gam, cost = cos)
# cos_list = append(cos_list, cos)
# gam_list = append(gam_list, gam)
# svm_predictions2 = predict(svm_model2, test_model[,-c(3)])
# model_name = append(model_name, "SVM2")
# print(paste(i, "of 36", sep = ""))
# print(rmse(actual = test_model$Global_Sales, predicted = svm_predictions2))
#   }
# }
svm_model2 = svm(Global_Sales~., train_model, kernel = "radial", gamma = 0.1, cost = 15)
svm_predictions2 = predict(svm_model2, test_model[,-c(3)])
model_name = append(model_name, "SVM_Radial")
#rmse(actual = test_model$Global_Sales, predicted = svm_predictions1)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = svm_predictions2))


#decision tree. 
library(rpart)
decision_model = rpart(Global_Sales~., train_model)
tree_prediction = predict(decision_model, test_model[,-c(3)])
model_name = append(model_name, "Decision_Tree")
#rmse(actual = test_model$Global_Sales, predicted = tree_prediction)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = tree_prediction))
plot(decision_model)
text(decision_model)

#random forest
library(randomForest)
rfmodel = randomForest(Global_Sales~., train_model, ntree = 100, mtry = 7, nodesize = 15)
rfpred = predict(rfmodel, test_model)
#rmse(actual = test_model$Global_Sales, predicted = rfpred)
model_name = append(model_name, "RF")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = rfpred))
varImpPlot(rfmodel)


train_matrix = as.matrix(std_train[,-c(93)])
library(xgboost)
xgb = xgb.cv(data = train_matrix, nrounds = 9999, metrics = "rmse", label = std_train$Global_Sales,
             print_every_n = 50, early_stopping_rounds = 50, objective = "reg:linear", eta = 0.1, max_depth = 4,
             nthread = 3, nfold = 10)



xgbmodel = xgboost(data = train_matrix, label = train_model$Global_Sales, eta = 0.1,
                   max_depth = 4, objective = "reg:linear", eval_metric = "rmse", verbose = 0,nrounds = 500)
dtest = as.matrix(std_test[,-c(93)])
xgbpredictions = predict(xgbmodel, dtest)
rmse(actual = test_model$Global_Sales, predicted = xgbpredictions)
model_name = append(model_name, "XGBoost")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = xgbpredictions))
xgb_importance = head(xgb.importance(feature_names = colnames(train_matrix),xgbmodel),10)
xgb.plot.importance(importance_matrix = xgb_importance)

result_df = data.frame(model_name, error)
write.csv(result_df, "results/results_humanfeatures.csv", row.names = F)

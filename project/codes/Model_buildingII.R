rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/project")

train = read.csv("data/after_humanfeaturegen/train.csv")[,-c(18,19)]
test = read.csv("data/after_humanfeaturegen/test.csv")[,-c(18,19)]


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
train_cat_index = c(1,2,6,13,14,15)#,16,17)
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
test_cat_index = c(1,2,6,13,14,15)#,16,17)
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

std_train = data.frame(lapply(std_train, function(x) as.numeric(as.character(x))))
std_test = data.frame(lapply(std_test, function(x) as.numeric(as.character(x))))
library(h2o)
localh2o = h2o.init(ip='localhost', port = 54321, max_mem_size = '6g',nthreads = 1)
train.hex = as.h2o(std_train)
test.hex = as.h2o(std_test)
aec = h2o.deeplearning(x = setdiff(colnames(train.hex), "Global_Sales"), 
                        y = "Global_Sales", training_frame = train.hex,
                        activation = "RectifierWithDropout",
                        hidden = c(25,20,10), epochs = 100)

features = as.data.frame(h2o.deepfeatures(aec, train.hex[,-93], layer = 1))
features = cbind(features, as.data.frame(h2o.deepfeatures(aec, train.hex[,-93], layer = 2)))
features = cbind(features, as.data.frame(h2o.deepfeatures(aec, train.hex[,-93], layer = 3)))

features2 = as.data.frame(h2o.deepfeatures(aec, test.hex[,-93], layer = 1))
features2 = cbind(features2, as.data.frame(h2o.deepfeatures(aec, test.hex[,-93], layer = 2)))
features2 = cbind(features2, as.data.frame(h2o.deepfeatures(aec, test.hex[,-93], layer = 3)))

model = c()
error = c()
features$Global_Sales = std_train$Global_Sales
features2$Global_Sales = std_test$Global_Sales
glm_model = glm(Global_Sales~., features, family = "gaussian")
glm_pred = predict(glm_model, features2[,-c(56)])
rmse(actual = test_model$Global_Sales, predicted = glm_pred)
model = append(model, "Regression")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = glm_pred))

#knn
library(FNN)
knn_pred = knn.reg(train = std_train[,-c(56)], test = std_test[,-c(56)], 
                   y = std_train$Global_Sales, k = 3)
rmse(actual = std_test$Global_Sales, predicted = knn_pred$pred)
model = append(model, "KNN")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = knn_pred$pred))


library(e1071)
svm_model = svm(Global_Sales~., features)
svm_predictions = predict(svm_model, features2[,-c(56)])
model = append(model, "SVM")
rmse(actual = test_model$Global_Sales, predicted = svm_predictions)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = svm_predictions))


svm_model1 = svm(Global_Sales~., features, kernel = "linear")
svm_predictions1 = predict(svm_model1, features2[,-c(56)])
model = append(model, "SVM_linear")
rmse(actual = test_model$Global_Sales, predicted = svm_predictions1)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = svm_predictions1))


svm_model2 = svm(Global_Sales~., features, kernel = "radial", gamma = 0.1, cost = 15)
svm_predictions2 = predict(svm_model2, features2[,-c(56)])
model = append(model, "SVM_radial")
rmse(actual = test_model$Global_Sales, predicted = svm_predictions2)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = svm_predictions2))


#decision tree. 
library(rpart)
decision_model = rpart(Global_Sales~., features)
tree_prediction = predict(decision_model, features2[,-c(56)])
model = append(model, "Decision_Tree")
rmse(actual = test_model$Global_Sales, predicted = tree_prediction)
error = append(error, rmse(actual = test_model$Global_Sales, predicted = tree_prediction))


#random forest
library(randomForest)
rfmodel = randomForest(Global_Sales~., features, ntree = 100, mtry = 7, nodesize = 15)
rfpred = predict(rfmodel, features2)
rmse(actual = test_model$Global_Sales, predicted = rfpred)
model = append(model, "RF")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = rfpred))


#xgboost
train_matrix = as.matrix(features[,-c(56)])
library(xgboost)
#The feautres from neural nets seems so learnable that the same hyper parameters seem to be causing 
#overfitting. 
xgbmodel = xgboost(data = train_matrix, label = features$Global_Sales, eta = 0.001, booster = "gbtree", 
                   max_depth = 2, objective = "reg:linear", eval_metric = "rmse", verbose = 0, nrounds = 100)
dtest = as.matrix(std_test[,-c(56)])
xgbpredictions = predict(xgbmodel, dtest)
rmse(actual = test_model$Global_Sales, predicted = xgbpredictions)
model = append(model, "XGBoost")
error = append(error, rmse(actual = test_model$Global_Sales, predicted = xgbpredictions))


results = data.frame(model, error)
write.csv(results, "results/results_neuralnetfeautres.csv", row.names = F)

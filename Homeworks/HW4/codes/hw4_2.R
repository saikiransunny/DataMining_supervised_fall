rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW4/codes")

data = read.table("wine.data.txt",sep = ",")
names(data) = c("Wine", "Alcohol", "Malic_acid", "Ash", "Alcalinity_ash", "Magnesium", "Total_phenols", "Flavanoids", 
                "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity","Hue","OD280/OD315","Proline")
str(data)
sum(is.na(data))
summary(data)

data$Wine = as.factor(as.character(data$Wine))

set.seed(1)
train_index = sample(1:nrow(data), 0.8*nrow(data))
train = data[train_index,]
test = data[-train_index,]


library(rpart)				    
library(partykit)
library(Metrics)
tree_model = rpart(Wine~., train, method = "class")
fancyRpartPlot(tree_model)	

#train predictions
train_predictions = predict(tree_model, train, type = "class")
table = table(train_predictions, train$Wine)
train_accuracy = (table[1,1]+table[2,2]+table[3,3]) / (sum(table[1,])+sum(table[2,])+sum(table[3,])) * 100
rnames_train = rownames(tree_model$frame)[tree_model$where]
node_4_train = sum(rnames_train == 4)
node_6_train = sum(rnames_train == 6)
node_5_train = sum(rnames_train == 5)
node_7_train = sum(rnames_train == 7)

#test predictions
test_predictions = predict(tree_model, test, type = "class")
table = table(test_predictions, test$Wine)
test_accuracy = (table[1,1]+table[2,2]+table[3,3]) / (sum(table[1,])+sum(table[2,])+sum(table[3,])) * 100

train_accuracy
test_accuracy
#test predictions node collection. 
node_5_test = 0

#class = 1
node_4_test = sum(test_predictions == 1)

#class = 2
node_6_test = sum(test_predictions == 2)

#node 5 vs 15. class = 3
index = which(test_predictions == 3)
for(i in index){
  if(test[i,]$Proline >= 755){
    node_5_test = node_5_test + 1
  }
}
node_7_test = length(index) - node_5_test
#analysis:
# In the training set 
#node4 = 47
#node5 = 7
#node6 = 51
#node7 = 37

#In the testing set
#node4 = 12
#node5 = 1
#node6 = 11
#node7 = 12


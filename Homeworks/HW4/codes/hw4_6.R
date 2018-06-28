rm(list = ls(all = T))

setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW4/codes")
data = read.csv("voice.csv")
set.seed(1)
train_index = sample(1:nrow(data), 0.8*nrow(data))
train = data[train_index,]
test = data[-train_index,]

outlier_train = train
set.seed(123)
row = sample(1:nrow(data),1)
col = sample(1:ncol(data)-1,1) #col 16 is selected
boxplot(train[,c(16)], main = "Boxplot")
outlier_train[row, col] = 5
boxplot(outlier_train[,c(16)], main = "Boxplot with outlier")
#clearly an outlier. 


library(h2o)
library(MLmetrics)
localh2o = h2o.init(ip='localhost', port = 54321, max_mem_size = '6g',nthreads = 1)
train.hex = as.h2o(train)
test.hex = as.h2o(test)

#check how many neurons is best 
acc = c()
for(i in seq(1,20,1)){
NN_model = h2o.deeplearning(x = setdiff(colnames(train.hex), "label"), 
                       y = "label", training_frame = train.hex,
                       activation = "RectifierWithDropout",
                       hidden = c(i), epochs = 100)
prediction = as.data.frame(h2o.predict(NN_model, newdata = test.hex[,-c(21)]))$predict
accuracy = MLmetrics::Accuracy(prediction, test$label)
acc = append(acc, accuracy)
print(i)
}
acc



#on the outlier train data. 
outlier_train.hex = as.h2o(outlier_train)
#check how many neurons is best 
acc = c()
for(i in seq(1,20,1)){
  NN_model = h2o.deeplearning(x = setdiff(colnames(outlier_train.hex), "label"), 
                              y = "label", training_frame = outlier_train.hex,
                              activation = "RectifierWithDropout",
                              hidden = c(i), epochs = 100)
  prediction = as.data.frame(h2o.predict(NN_model, newdata = test.hex[,-c(21)]))$predict
  accuracy = MLmetrics::Accuracy(prediction, test$label)
  acc = append(acc, accuracy)
  print(i)
}
acc





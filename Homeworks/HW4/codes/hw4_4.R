library(kernlab)

data("spam")

#data sanity check. 
sum(is.na(spam))

names(spam)

#split train and test. 
set.seed(1234)
train_index = sample(1:nrow(spam), 0.8*nrow(spam))
train = spam[train_index,]
test = spam[-train_index,]


library(randomForest)
library(MLmetrics)
mtrylist = c()
accuracylist = c()
#select the best m. 
######
for(i in seq(5,55,5)){
rf.model = randomForest(type~., spam, ntree = 100, mtry = i)
model_predictions = predict(rf.model, test)

mtrylist = append(mtrylist, i)
accuracylist = append(accuracylist, MLmetrics::Accuracy(model_predictions, test$type))
print(i)
}
######
accuracylist


#best accuracy is obtained with mtry = 15. Tuning it much more -- iterating with smaller width. 
######
mtrylist = c()
accuracylist = c()
#select the best m. 
for(i in seq(2,15,2)){
  rf.model = randomForest(type~., spam, ntree = 100, mtry = i)
  model_predictions = predict(rf.model, test)
  
  mtrylist = append(mtrylist, i)
  accuracylist = append(accuracylist, MLmetrics::Accuracy(model_predictions, test$type))
  print(i)
}
######
accuracylist



#choosing mtry = [2,4,6,8,10] for plotting. 
######
mtrylist = c()
OOBlist = rep(0,100)
test_error = c()
#select the best m. 
for(i in c(2,4,6,8,10)){
  rf.model = randomForest(type~., spam, ntree = 100, mtry = i)
  model_predictions = predict(rf.model, test)
  
  OOBlist = cbind(OOBlist, rf.model$err.rate[,c(1)])
  mtrylist = append(mtrylist, i)
  test_error = append(test_error, MLmetrics::Accuracy(model_predictions, test$type))
  print(i)
}
######

plot_dataframe = data.frame(mtrylist, test_error)
names(plot_dataframe) = c("mtry", "accuracy")
library(ggplot2)
ggplot(plot_dataframe, aes(x = mtry, y = accuracy)) + geom_line() + geom_point() +
  ggtitle("Mtry vs Accuracy") + theme(plot.title = element_text(hjust = 0.5))



#out of bag errors. 
OOBlist_plot = data.frame(OOBlist)[,-c(1)]
names(OOBlist_plot) = c("mtry_2", "mtry_4","mtry_6","mtry_8","mtry_10")
OOBlist_plot$NTrees = seq(1, 100)
#plotting 
ggplot(OOBlist_plot, aes(NTrees)) + 
  geom_line(aes(y = mtry_2, colour = "mtry_2")) + geom_point(aes(y = mtry_2), size = 0.3) + 
  geom_line(aes(y = mtry_4, colour = "mtry_4")) + geom_point(aes(y = mtry_4), size = 0.3) + 
  geom_line(aes(y = mtry_6, colour = "mtry_6")) + geom_point(aes(y = mtry_6), size = 0.3) + 
  geom_line(aes(y = mtry_8, colour = "mtry_8")) + geom_point(aes(y = mtry_8), size = 0.3) + 
  geom_line(aes(y = mtry_10, colour = "mtry_10")) + geom_point(aes(y = mtry_10), size = 0.3) + 
  
  ggtitle("Out of Bag Errors.") + xlab("No of features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))


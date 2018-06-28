rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW3/codes")


data = read.table("DiabetesAndrews36_1.txt")
data = data[5:10]
names(data) = c("glucose_area", "insulin_area", "SSPG", "relative_weight", "fasting_plasma_glucose", "target")

data$target = as.factor(as.character(data$target))

sum(is.na(data))
#pair plot. 
library(ggplot2)
library(GGally)
ggpairs(data, columns = 1:5)
ggpairs(data, columns = 1:5, ggplot2::aes(colour=target))


####################
#
#LDA and QDA models
####################
#train and test creation. 
set.seed(1234)
train_index = sample(1:nrow(data), 0.7*nrow(data))
train = data[train_index,]
test = data[-train_index,]


#get least squres. 
get_LeastSquares = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + ((Truevalues[i] - Predictedvalues[i])^2)
  }
  #print(sum)
  return(sum)
}

#LDA model
lda_model = lda(target~., train)
lda_predictions = predict(lda_model, test[,1:5])$class
lda_error = get_LeastSquares(as.numeric(as.character(test$target)), as.numeric(as.character(lda_predictions)))

#QDA model
qda_model = qda(target~., train)
qda_predictions = predict(qda_model, test[,1:5])$class
qda_error = get_LeastSquares(as.numeric(as.character(test$target)), as.numeric(as.character(qda_predictions)))
lda_error
qda_error
#lda_error = 10
#qda_error = 3
#Error from qda is less. Hence it is better for this data. 


#c
final_vector = data.frame(0.98, 122, 544, 186, 184)
names(final_vector) = names(train)[1:5]


#lda prediction. 
predict(lda_model, final_vector)$class

#qda prediction.
predict(qda_model, final_vector)$class

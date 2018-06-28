rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW4/codes")

library(lasso2)
prostate = data(Prostate)

#check the sanity of the data. 
sum(is.na(Prostate))

#divide train and test. 
set.seed(1234)
train_index = sample(1:nrow(Prostate), 0.8*nrow(Prostate))
train = Prostate[train_index,]
test = Prostate[-train_index,]


library(leaps)
regfit.full <- regsubsets(lcavol~., data = train, nbest = 1, nvmax = 8, method = "exhaustive")
my_sum <- summary(regfit.full)
my_sum

#2 feature regression is best. 
which(my_sum$cp == min(my_sum$cp)) 


#lcp and lpsa are the best 2 feature subset. 
which(my_sum$outmat[2,] == "*")


library(stats)
AIC_val = c()
BIC_val = c()
for (i in 1:8){
  temp = which(my_sum$outmat[i,] == "*")
  temp = temp + 1
  
  train_subset = train[, c(1,temp)]
  glm_model = glm(lcavol~., data = train_subset, family = "gaussian")
 
  AIC_val = append(AIC_val, AIC(glm_model))
  BIC_val = append(BIC_val, BIC(glm_model))
}
features = seq(1,8)
plot_dataframe = data.frame(features, AIC_val, BIC_val)
library(ggplot2)
#plotting 
ggplot(plot_dataframe, aes(features)) + 
  geom_line(aes(y = AIC_val, colour = "AIC_val")) + 
  geom_line(aes(y = BIC_val, colour = "BIC_val")) +
  ggtitle("AIC vs BIC") + xlab("No of features") + ylab("values") +
  theme(plot.title = element_text(hjust = 0.5))




CV = function(dataframe, fold = 5){
dataframe<-dataframe[sample(nrow(dataframe)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(dataframe)),breaks=fold,labels=FALSE)
#Perform 10 fold cross validation
results = c()
for(i in 2:fold){
  #Segement your data by fold using the which() function 
  testIndexes = which(folds==i,arr.ind=TRUE)
  testData = dataframe[testIndexes, ]
  trainData = dataframe[-testIndexes, ]

  #train and test with glm. 
  glm_model = glm(lcavol~., trainData, family = "gaussian")
  predictions = predict(glm_model, testData)
  results = append(results, (1/length(predictions))*sum((predictions - testData$lcavol)^2))
}
return(mean(results))
}

#5CV
#####
train_error_5cv = c()
test_error_5cv = c()
for (i in 1:8){
  temp = which(my_sum$outmat[i,] == "*")
  temp = temp + 1
  
  red.training = train[, c(1,temp)]
  red.testing = test[,c(1,temp)]
  
  red.fit = glm(lcavol~., data = red.training, family = "gaussian")
  
  pred.test = predict(red.fit, red.testing)
  
  test.error = (1/length(red.testing$lcavol))*sum((pred.test - red.testing$lcavol)^2)
  train.error = CV(red.training, 5) 
  
  train_error_5cv = append(train_error_5cv, train.error)
  test_error_5cv = append(test_error_5cv, test.error)
  
}
#####


#10CV
#####
train_error_10cv = c()
test_error_10cv = c()
for (i in 1:8){
  temp = which(my_sum$outmat[i,] == "*")
  temp = temp + 1
  
  red.training = train[, c(1,temp)]
  red.testing = test[,c(1,temp)]
  
  red.fit = glm(lcavol~., data = red.training, family = "gaussian")
  
  pred.test = predict(red.fit, red.testing)
  
  test.error = (1/length(red.testing$lcavol))*sum((pred.test - red.testing$lcavol)^2)
  train.error = CV(red.training, 10) 
  
  train_error_10cv = append(train_error_10cv, train.error)
  test_error_10cv = append(test_error_10cv, test.error)
  
}
#####
# create functions that feed into "bootpred"
beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create X and Y
X = train[,2:9]
Y = train[,1]

# Generalize it, and search over the best possible subsets of size "k"
boot_error = c()
for (i in 1:8){
  # Pull out the model
  temp = which(my_sum$outmat[i,] == "*")
  
  res = bootpred(X[,temp], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  boot_error = c(boot_error, res[[3]])
  
}



library(ggplot2)
features = seq(1,8)
plot_dataframe = data.frame(features, train_error_5cv, test_error_5cv, train_error_10cv, test_error_10cv, 
                            boot_error)

#plotting 
ggplot(plot_dataframe, aes(features)) + 
  geom_line(aes(y = train_error_5cv, colour = "train_error_5cv")) + geom_point(aes(y = train_error_5cv),
                                                                               size = 1.5) + 
  geom_line(aes(y = test_error_5cv, colour = "test_error_5cv")) + geom_point(aes(y = test_error_5cv),
                                                                             size = 1.5) + 
  
  geom_line(aes(y = train_error_10cv, colour = "train_error_10cv")) + geom_point(aes(y = train_error_10cv),
                                                                                 size = 1.5) + 
  geom_line(aes(y = test_error_10cv, colour = "test_error_10cv")) + geom_point(aes(y = test_error_10cv),
                                                                               size = 1.5) + 
  
  geom_line(aes(y = boot_error, colour = "boot_error")) + geom_point(aes(y = boot_error),size = 1.5) + 
  
  ggtitle("Different errors.") + xlab("No of features") + ylab("MSE") +
  theme(plot.title = element_text(hjust = 0.5))



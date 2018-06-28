########################
#Clean the work station.
########################
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW2")
######
#Q1
######

library(ISLR)
data("College")
college_data = College
rm(College)

################################
#Basic analysis about the college_data. 
################################
names(college_data)
head(college_data)
sum(is.na(college_data))

#Looks like the college name is present as the rowname. It would be better if that is added as a feature. 
#But we would not use this feature for regression. It's just for data analysis. 
college_data$college_name = rownames(college_data)

#Some of the features are numeric though they are categorical. Let's convert them into categorical features. 
college_data$Private = as.factor(as.character(college_data$Private))
college_data$college_name = as.factor(as.character(college_data$college_name))

summary(college_data)

#Let's create some new features that might give some insight. 
college_data$acceptance_rate = college_data$Apps / college_data$Accept
college_data$not_enrolled = college_data$Apps - college_data$Enroll
college_data$total_fee = college_data$Outstate + college_data$Room.Board + college_data$Books + college_data$Personal
summary(college_data)


#There seem to be a few features whose range is in thousands and others are in 100s. 
#Let change the scale of the thousand features'
college_data$log_Accept = log(1+college_data$Accept)
college_data$log_Enroll = log(1+college_data$Enroll)
college_data$log_F.Undergrad = log(1+college_data$F.Undergrad)
college_data$log_P.Undergrad = log(1+college_data$P.Undergrad)
college_data$log_Outstate = log(1+college_data$Outstate)
college_data$log_Room.Board = log(1+college_data$Room.Board)
college_data$log_Books = log(1+college_data$Books)
college_data$log_Personal = log(1+college_data$Personal)
college_data$log_Expend = log(1+college_data$Expend)
college_data$log_not_enrolled = log(1+college_data$not_enrolled)
college_data$log_total_fee = log(1+college_data$total_fee)

summary(college_data)
#Remove the original features of the ones that went log transformation. 
college_data = subset(college_data, select = -c(Accept, Enroll, F.Undergrad, P.Undergrad, Outstate, Room.Board, 
                                                Books, Personal, Expend, not_enrolled, total_fee))

# IT IS IMPORTANT TO HAVE THE TRAINING AND TESTING DISTRIBUTIONS FOR ALL THE FEATURES TO BE SIMILAR. 
# HENCE WE ARE FIRST REMOVING THE OUTLIERS FROM THE DATA AND THEN DIVIDING THE DATA INTO TRAIN AND TEST. 
#################################
#Outlier removal. 
#################################

#Removing outliers by hand is a tedious process. Let's write a method which would do the same. 
outlier_removal = function(data, feature_index){
    outliers = unique(sort(boxplot.stats(data[,feature_index])$out))
    if(length(outliers) > 0){
      outlier_index = c()
      for(outlier in 1:length(outliers)){
        outlier_index = append(outlier_index, grep(outliers[outlier], 
                                                   data[,feature_index]))
      }
      outlier_index = unique(outlier_index)
      data = data[-outlier_index,]
    }
    return(data)
}
#log_accept
par(mfrow=c(1,2)) 
plot(density(college_data$log_Accept), main = "Density of Log_Acceptance.")
boxplot(college_data$log_Accept, main = "Boxplot of Log_Acceptance.")
#Normal distribution with a couple of outliers. 
col_index = grep(pattern = "log_Accept", names(college_data))
college_data = outlier_removal(college_data, col_index)

#log_Enroll
par(mfrow=c(1,2)) 
plot(density(college_data$log_Enroll), main = "Density of log_Enroll.")
boxplot(college_data$log_Enroll, main = "Boxplot of log_Enroll.")
# Normal distribution with a single outlier.
col_index = grep(pattern = "log_Enroll", names(college_data))
college_data = outlier_removal(college_data, col_index)

#top10perc
par(mfrow=c(1,2)) 
plot(density(college_data$Top10perc), main = "Density of Top10percentile.")
boxplot(college_data$Top10perc, main = "Boxplot of Top10percentile.")

#There are too many outliers to remove. Hence let's change the scale of this feature. 
college_data$log_Top10perc = log(1+college_data$Top10perc)
par(mfrow=c(1,2)) 
plot(density(college_data$log_Top10perc), main = "Density of log_Top10percentile.")
boxplot(college_data$log_Top10perc, main = "Boxplot of log_Top10percentile.")
#Lot less data to remove!
#Let's keep this and remove top10perc. 
college_data = subset(college_data, select = -c(Top10perc))
col_index = grep(pattern = "log_Top10perc", names(college_data))
college_data = outlier_removal(college_data, col_index)


#top25perc
par(mfrow=c(1,2)) 
plot(density(college_data$Top25perc), main = "Density of Top25percentile.")
boxplot(college_data$Top25perc, main = "Boxplot of Top25percentile.")
#Almost a perfect normal distribution with no outliers!

#log_f.undergrad
par(mfrow=c(1,2)) 
plot(density(college_data$log_F.Undergrad), main = "Density of log_F.Undergrad.")
boxplot(college_data$log_F.Undergrad, main = "Boxplot of log_F.Undergrad.")
# Normal distribution without any outliers. 

#log_p.undergrad
par(mfrow=c(1,2)) 
plot(density(college_data$log_P.Undergrad), main = "Density of log_P.Undergrad.")
boxplot(college_data$log_P.Undergrad, main = "Boxplot of log_P.Undergrad.")
#Normal distribution with few outliers. 
col_index = grep(pattern = "log_P.Undergrad", names(college_data))
college_data = outlier_removal(college_data, col_index)


#log_Outstate
par(mfrow=c(1,2)) 
plot(density(college_data$log_Outstate), main = "Density of log_Outstate.")
boxplot(college_data$log_Outstate, main = "Boxplot of log_Outstate.")
#Normal distribution with few outliers.
col_index = grep(pattern = "log_Outstate", names(college_data))
college_data = outlier_removal(college_data, col_index)

#log_room.board
par(mfrow=c(1,2)) 
plot(density(college_data$log_Room.Board), main = "Density of log_roomboard.")
boxplot(college_data$log_Room.Board, main = "Boxplot of log_roomboard.")
#Normal distribution with few outliers. Let's remove it.  
col_index = grep(pattern = "log_Room.Board", names(college_data))
college_data = outlier_removal(college_data, col_index)


#log_books
par(mfrow=c(1,2)) 
plot(density(college_data$log_Books), main = "Density of log_Books.")
boxplot(college_data$log_Books, main = "Boxplot of log_Books.")
#There are too many outliers for us to remove. We would loose a lot of data. 
#Let's plot a qqnormplot to further investigate. 
qqnorm(college_data$log_Books)
#The 3 outliers around 4.5 and 1 at 7.5 seem to be enough to remove from this plot. 
#Let's remove those manually. 
rm.index = which(college_data$log_Books >= 7.5) #values from the plot
rm.index = append(rm.index, which(college_data$log_Books <= 5))#values from the plot.
college_data = college_data[-rm.index,]


#log_personal
par(mfrow=c(1,2)) 
plot(density(college_data$log_Personal), main = "Density of log_Personal.")
boxplot(college_data$log_Personal, main = "Boxplot of log_Personal.")
#Normal distribution with few outliers.  
col_index = grep(pattern = "log_Personal", names(college_data))
college_data = outlier_removal(college_data, col_index)


#phd
par(mfrow=c(1,2)) 
plot(density(college_data$PhD), main = "Density of Phd.")
boxplot(college_data$PhD, main = "Boxplot of Phd.")
#Normal distribution with few outliers. 
col_index = grep(pattern = "PhD", names(college_data))
college_data = outlier_removal(college_data, col_index)


#terminal
par(mfrow=c(1,2)) 
plot(density(college_data$Terminal), main = "Density of Terminal.")
boxplot(college_data$Terminal, main = "Boxplot of Terminal.")
#Normal distribution with few outliers. 
col_index = grep(pattern = "Terminal", names(college_data))
college_data = outlier_removal(college_data, col_index)


#s.f.ratio
par(mfrow=c(1,2)) 
plot(density(college_data$S.F.Ratio), main = "Density of S.F.Ratio.")
boxplot(college_data$S.F.Ratio, main = "Boxplot of S.F.Ratio.")
#Too many outliers to remove. Let's look at qqnormplot 
qqnorm(college_data$S.F.Ratio)
#looking at the qqnormplot it seems optimal to remove the single outlier up in the top. 
rm.index = which(college_data$S.F.Ratio == max(college_data$S.F.Ratio))
college_data = college_data[-rm.index,]

#perc.alumni
par(mfrow=c(1,2)) 
plot(density(college_data$perc.alumni), main = "Density of Perc.Alumni.")
boxplot(college_data$perc.alumni, main = "Boxplot of Perc.Alumni.")
#Normal distribution with few outliers. 
col_index = grep(pattern = "perc.alumni", names(college_data))
college_data = outlier_removal(college_data, col_index)


#expend
par(mfrow=c(1,2)) 
plot(density(college_data$log_Expend), main = "Density of log_Expend.")
boxplot(college_data$log_Expend, main = "Boxplot of log_Expend.")
#Normal distribution with outliers. 
#plotting qqnormplot
qqnorm(college_data$log_Expend)
#There doesn't seem to be much classification betwwen the bunch of outliers. 
#Let's keep them since there are too many to remove.


#grad.rate
par(mfrow=c(1,2)) 
plot(density(college_data$Grad.Rate ), main = "Density of Grad_rate.")
boxplot(college_data$Grad.Rate, main = "Boxplot of Grad_rate.")
#Normal distribution with few outliers. 
col_index = grep(pattern = "Grad.Rate", names(college_data))
college_data = outlier_removal(college_data, col_index)


#acceptance_rate
par(mfrow=c(1,2)) 
plot(density(college_data$acceptance_rate), main = "Density of Acceptance_rate.")
boxplot(college_data$acceptance_rate, main = "Boxplot of Acceptance_rate.")
#Normal distribution with outliers. plotting qqnormplot since too many outliers to remove. 
qqnorm(college_data$acceptance_rate)
#Let's remove the ones greater than 4. 
rm.index = which(college_data$acceptance_rate >= 4)
college_data = college_data[-rm.index,]


#not_enrolled
par(mfrow=c(1,2)) 
plot(density(college_data$log_not_enrolled), main = "Density of log_Not_Enrolled.")
boxplot(college_data$log_not_enrolled, main = "Boxplot of log_Not_Enrolled.")
#Normal distribution 

#total_Fee
par(mfrow=c(1,2)) 
plot(density(college_data$log_total_fee), main = "Density of Total_Fee.")
boxplot(college_data$log_total_fee, main = "Boxplot of Total_Fee.")
#Normal distribution!
rm(col_index, rm.index)

#Let's plot the number of private and public universities in the college_data. 
library(ggplot2)
g = ggplot(college_data, aes(Private))
g + geom_bar() + ggtitle("Number of Private universities")

#To get a general idea about the trend in the college_dataset, let's do a pairs plot. 
library(GGally)
ggpairs(college_data[,-c(2,9)]) #Let's not include company name. Since there are so many.
#Looking at the general trend of the college_data, it looks like 
# The following seems to have interesting relationship. 
# (log_Enroll, log_accept)  #indicating that students are applying for the universities that only interest them. 
#(log_total_fee, log_outstate) indicating that most of the expenditure constitues tuition fee
rm(outliers, rm.index, g)


#Least squares 
get_LeastSquares = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + (Truevalues[i] - Predictedvalues[i])^2
  }
  print(sum)
  return(sum)
}


regressionmethod_list = c()
leastsquare_list = c()
#Get train and test datasets. 
train.index = sample(1:nrow(college_data), 0.7*nrow(college_data))
train = college_data[train.index,]
test = college_data[-train.index,]
library(glmnet)
#linear model. 
linear_model = lm(Apps~., train[,-c(9)])
summary(linear_model)
linear_predictions = predict(linear_model, test[,-c(9, 2)])
result = get_LeastSquares(Truevalues = test$Apps, linear_predictions)
regressionmethod_list = append(regressionmethod_list, "linear_regression")
leastsquare_list = append(leastsquare_list, result)


#ridge model
#ridge model needs all the numeric features, hence converting 'private' feature into numeric. 
train$Private = sapply(train$Private, function(x) ifelse(x == "Yes", 1, 0))
test$Private = sapply(test$Private, function(x) ifelse(x == "Yes", 1, 0))

ridge_model.cv = cv.glmnet(x = as.matrix(train[,-c(2, 9)]), y = train$Apps, alpha = 0)
plot(ridge_model.cv)
best_lambda = ridge_model.cv$lambda.min
ridge_predictions = predict(ridge_model.cv, s = best_lambda, newx = as.matrix(test[,-c(2, 9)]),
                            type =  "response")
result = get_LeastSquares(Truevalues = test$Apps, ridge_predictions)
regressionmethod_list = append(regressionmethod_list, "RidgeModel")
leastsquare_list = append(leastsquare_list, result)


#lasso model
lasso_model.cv = cv.glmnet(x = as.matrix(train[,-c(2, 9)]), y = train$Apps, alpha = 1)
plot(lasso_model.cv)
best_lambda = lasso_model.cv$lambda.min
lasso_predictions = predict(lasso_model.cv, s = best_lambda, newx = as.matrix(test[,-c(2, 9)]),
                            type =  "response")
result = get_LeastSquares(Truevalues = test$Apps, lasso_predictions)
regressionmethod_list = append(regressionmethod_list, "LassoModel")
leastsquare_list = append(leastsquare_list, result)


#pcr
library(pls)
set.seed(1234)
pcr_model.cv = pcr(Apps~., data = train[,-c(9)], scale = TRUE, validation = "CV")
summary(pcr_model.cv)
validationplot(pcr_model.cv, val.type = "MSEP")
#selecting the optimal number of principal components from regression. 
i_list = c()
pcr_errorlist = c()
for(i in 1:length(pcr_model.cv$Xvar)){
  i_list = append(i_list, i)
  predictions = predict(pcr_model.cv, test[,-c(2, 9)], ncomp = i)
  error = get_LeastSquares(Truevalues = test$Apps, Predictedvalues = predictions)
  pcr_errorlist = append(pcr_errorlist, error)
}
pcr_plotdata = data.frame(i_list, pcr_errorlist)
#Let's plot the principle components and their errors. 
g = ggplot(pcr_plotdata, aes(i_list, pcr_errorlist))
g + geom_bar(stat = "identity") + ggtitle("PCR error variation by principle components")

regressionmethod_list = append(regressionmethod_list, "pcr")
leastsquare_list = append(leastsquare_list, min(pcr_errorlist))


#pls
pls_regression = plsr(Apps~., data = train[,-c(9)], scale = TRUE, validation = "CV")
summary(pls_regression)
validationplot(pls_regression, val.type = "MSEP")
i_list = c()
pls_errorlist = c()
for(i in 1:length(pls_regression$Xvar)){
  i_list = append(i_list, i)
  predictions = predict(pls_regression, test[,-c(2, 9)], ncomp = i)
  error = get_LeastSquares(Truevalues = test$Apps, Predictedvalues = predictions)
  pls_errorlist = append(pls_errorlist, error)
}
pls_plotdata = data.frame(i_list, pls_errorlist)
#Let's plot the principle components and their errors. 
g = ggplot(pls_plotdata, aes(i_list, pls_errorlist))
g + geom_bar(stat = "identity") + ggtitle("PLS error variation by principle components")

regressionmethod_list = append(regressionmethod_list, "pls")
leastsquare_list = append(leastsquare_list, min(pls_errorlist))

options("scipen"=100, "digits"=4)
Final_comparision = data.frame(regressionmethod_list, leastsquare_list)
Final_comparision$regressionmethod_list = as.character(Final_comparision$regressionmethod_list)
Final_comparision$regressionmethod_list = factor(Final_comparision$regressionmethod_list,
                                                  levels = Final_comparision$regressionmethod_list)
g = ggplot(Final_comparision, aes(regressionmethod_list, leastsquare_list))
g + geom_bar(stat = "identity") + ggtitle("Different method's error comparison")


rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/project")
data_project = read.csv("data/data.csv")

names(data)

#split train and test data.
####
set.seed(1234)
training_data_rowindex = sample(1:nrow(data), 0.8*nrow(data))
training_data = data[training_data_rowindex,]
test_data = data[-training_data_rowindex,]

write.csv(training_data, "data/training_data.csv", row.names = F)
write.csv(test_data, "data/test_data.csv", row.names = F)
####
training_data = read.csv("data/training_data.csv")
test_data = read.csv("data/test_data.csv")

# rm(data)
summary(training_data)

#feature type correction. 
training_data$User_Score = as.numeric(as.character(training_data$User_Score))
test_data$User_Score = as.numeric(as.character(test_data$User_Score))


cor(training_data[,grep("Sales", names(training_data))])
#remove the columns NA_Sales, EU_Sales and Developer.  
#####
remove_cols = function(dataframe){
  return(subset(dataframe, select = -c(Developer, NA_Sales, EU_Sales)))
}
training_data = remove_cols(training_data)
test_data = remove_cols(test_data)
#####





# All kinds of data imputation.  
#####

#check Na or blanks and replace. 

#check blanks and impute. 
#####
check_blank_sum = function(dataframe){
  for(i in 1:ncol(dataframe)){
    print(paste(names(dataframe)[i], length(which(dataframe[,i] == ""))))
  }
}
check_blank_sum(training_data)
check_blank_sum(test_data)


#replacemnets. (Imputations.)
#Both train and test has a blank column where there is no genre. 
#replacing it with the high count genre - action
#####
genre_index = which(training_data$Genre == "")
training_data$Genre[genre_index] = "Action"
table(training_data$Genre)

table(test_data$Genre)
genre_index = which(test_data$Genre == "")
test_data$Genre[genre_index] = "Action"
table(test_data$Genre)
#####


#removing the row of train and test where there is no name. 
#cannot impute since every game name is unique. 
#####
rm.index = which(training_data$Name == "")
training_data = training_data[-rm.index,]

rm.index = which(test_data$Name == "")
test_data = test_data[-rm.index,]
#####


check_blank_sum(training_data)
check_blank_sum(test_data)


#Imputing rating. 
Rating_imputation = function(dataframe){
  dataframe$Rating = as.character(dataframe$Rating)
  rating_index = which(dataframe$Rating == "")
  dataframe$Rating[rating_index] = "Unknown"
  dataframe$Rating = as.factor(as.character(dataframe$Rating))
  dataframe$Rating = as.factor(as.character(dataframe$Rating))
  
  return(dataframe)
}
table(training_data$Rating)
training_data = Rating_imputation(training_data)
test_data = Rating_imputation(test_data)
table(training_data$Rating)
table(test_data$Rating)
#####

#check nas and impute. 
#####
check_sum_na = function(dataframe){
  for(i in 1:ncol(dataframe)){
    print(paste(names(dataframe)[i], sum(is.na(dataframe[,i]))))
  }
}
check_sum_na(training_data)
check_sum_na(test_data)

par(mfrow=c(2,2)) 
plot(density(training_data$Critic_Count, na.rm = T), main = "Critic_Count")
plot(density(training_data$Critic_Score, na.rm = T), main = "Critic_Score")
plot(density(training_data$User_Count, na.rm = T), main = "User_Count")
plot(density(training_data$User_Score, na.rm = T), main = "User_Score")
#all the plots have tails. hence using median is better. 

#instead of imputing with mean or median 
#grouping and then imputing is a better idea!
#data imputation using grouping. 
library(dplyr)
training_data %>% group_by(Genre) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T),
            mean_critic_count = median(Critic_Count, na.rm = T),
            mean_user_count = median(User_Count, na.rm = T),
            mean_user_score = median(User_Score, na.rm = T))


na_imputation = function(dataframe, feature, replacement, genre){
  col_index = grep(feature, names(dataframe))
  na.index = which(is.na(dataframe[,col_index]))
  genre.index = which(dataframe$Genre == genre)
  
  common.index = intersect(na.index, genre.index)
  dataframe[common.index,col_index] = replacement
  
  return(dataframe)
}
genre_list = sort(unique(training_data$Genre))
critic_score_list = c(68, 66, 72, 68.5, 69, 70, 69, 74, 73, 70, 75, 74)
critic_count_list = c(22,16,24,17,17,14,18,30,32,17,18,24)
user_count_list = c(26, 15, 27, 13, 22, 11, 17, 51.5, 62, 20, 16, 35.5)
user_score_list = c(7.4, 7.5, 7.7, 7.0, 7.7, 7.5, 7.4, 7.9, 7.4, 7.6, 7.4, 7.9)
for(i in 1:length(genre_list)){
  training_data = na_imputation(training_data, "Critic_Score", critic_score_list[i], genre_list[i])
  training_data = na_imputation(training_data, "Critic_Count", critic_count_list[i], genre_list[i])
  training_data = na_imputation(training_data, "User_Count", user_count_list[i], genre_list[i])
  training_data = na_imputation(training_data, "User_Score", user_score_list[i], genre_list[i])
}
#####
check_sum_na(training_data)
check_sum_na(test_data)


test_data %>% group_by(Genre) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T),
            mean_critic_count = median(Critic_Count, na.rm = T),
            mean_user_count = median(User_Count, na.rm = T),
            mean_user_score = median(User_Score, na.rm = T))

critic_score_list = c(68, 66, 72, 71, 70, 70, 71, 73, 74, 68, 75, 72)
critic_count_list = c(26,21,26,15,19,15.5,16,27,29,17,18,26)
user_count_list = c(34, 21, 24.5, 13, 27, 20, 18, 47, 53, 19, 15, 35)
user_score_list = c(7.2, 7.8, 7.5, 7.5, 7.7, 7.1, 7.4, 7.8, 7.5, 7.3, 7.4, 7.5)
for(i in 1:length(genre_list)){
  test_data = na_imputation(test_data, "Critic_Score", critic_score_list[i], genre_list[i])
  test_data = na_imputation(test_data, "Critic_Count", critic_count_list[i], genre_list[i])
  test_data = na_imputation(test_data, "User_Count", user_count_list[i], genre_list[i])
  test_data = na_imputation(test_data, "User_Score", user_score_list[i], genre_list[i])
}
check_sum_na(training_data)
check_sum_na(test_data)




#Final check if things are consistent with train and test. 
##### 
sort(unique(training_data$Platform))
sort(unique(test_data$Platform))
#Additional PCFX category in test. Have to deal with it later on. 
sort(unique(training_data$Year_of_Release))
sort(unique(test_data$Year_of_Release))

sort(unique(training_data$Genre))
sort(unique(test_data$Genre))

sort(unique(training_data$Rating))
sort(unique(test_data$Rating))

#####



str(training_data)
#data analysis
cbrt = function(x) {
  sign(x) * abs(x)^(1/3)
}
fr_rt = function(x) {
  sign(x) * abs(x)^(1/4)
}
feature_transformations = function(vector){
  par(mfrow = c(2,2))
  qqnorm(vector, main = "Original Feature")
  qqnorm(sapply(vector, function(x) sqrt(x)), main = "Square Root")
  qqnorm(sapply(vector, function(x) x^2), main = "Squared")
  qqnorm(sapply(vector, function(x) log(1+x)), main = "Log")
  
}
#Japan sales. 
par(mfrow=c(1,2)) 
plot(density(training_data$JP_Sales), main = "Density of JP_Sales.")
boxplot(training_data$JP_Sales, main = "Boxplot of JP_Sales.")
feature_transformations(training_data$JP_Sales)
#Log transformation is better. 
training_data$log_JP_Sales = sapply(training_data$JP_Sales, function(x) log(1+x))
test_data$log_JP_Sales = sapply(test_data$JP_Sales, function(x) log(1+x))
training_data = subset(training_data, select = -c(JP_Sales))
test_data = subset(test_data, select = -c(JP_Sales))
rm.index = which(training_data$log_JP_Sales > 1.7)
training_data = training_data[-rm.index,]



#Other sales. 
par(mfrow=c(1,2)) 
plot(density(training_data$Other_Sales), main = "Density of Other_Sales")
boxplot(training_data$Other_Sales, main = "Boxplot of Other_Sales")
feature_transformations(training_data$Other_Sales)
#log transformation is better. 
training_data$log_Other_Sales = sapply(training_data$Other_Sales, function(x) log(1+x))
test_data$log_Other_Sales = sapply(test_data$Other_Sales, function(x) log(1+x))
training_data = subset(training_data, select = -c(Other_Sales))
test_data = subset(test_data, select = -c(Other_Sales))
rm.index = which(training_data$log_Other_Sales > 1.2)
training_data = training_data[-rm.index,]


#Critic score
par(mfrow=c(1,2)) 
plot(density(training_data$Critic_Score), main = "Density of Critic_Score")
boxplot(training_data$Critic_Score, main = "Boxplot of Critic_Score")
feature_transformations(training_data$Critic_Score)
rm.index = which(training_data$Critic_Score < 15)
training_data = training_data[-rm.index,]


#Critic count. 
par(mfrow=c(1,2)) 
plot(density(training_data$Critic_Count), main = "Density of Critic_Count")
boxplot(training_data$Critic_Count, main = "Boxplot of Critic_Count")
feature_transformations(training_data$Critic_Count)
#square root is more appropriate as a feature transformation. 
training_data$sqrt_Critic_Count = sapply(training_data$Critic_Count, function(x) sqrt(x))
test_data$sqrt_Critic_Count = sapply(test_data$Critic_Count, function(x) sqrt(x))
training_data = subset(training_data, select = -c(Critic_Count))
test_data = subset(test_data, select = -c(Critic_Count))
rm.index = which(training_data$sqrt_Critic_Count  < 2)
rm.index = append(rm.index, which(training_data$sqrt_Critic_Count > 10))
training_data = training_data[-rm.index,]


#user score
par(mfrow=c(1,2)) 
plot(density(training_data$User_Score), main = "Density of User_Score")
boxplot(training_data$User_Score, main = "Boxplot of User_Score")
feature_transformations(training_data$User_Score)


#user count. 
par(mfrow=c(1,2)) 
plot(density(training_data$User_Count), main = "Density of User_Count")
boxplot(training_data$User_Count, main = "Boxplot of User_Count")
feature_transformations(training_data$User_Count)
#log transformations. 
training_data$log_User_Count = sapply(training_data$User_Count, function(x) log(1+x))
test_data$log_User_Count = sapply(test_data$User_Count, function(x) log(1+x))
training_data = subset(training_data, select = -c(User_Count))
test_data = subset(test_data, select = -c(User_Count))


write.csv(training_data, "data/after_outlierandtrasformation/train.csv", row.names = F)
write.csv(test_data, "data/after_outlierandtrasformation/test.csv", row.names = F)

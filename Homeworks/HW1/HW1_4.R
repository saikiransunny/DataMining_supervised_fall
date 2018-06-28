#####
#Q4
#####
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW1")

library(MASS)
data("Boston")
Boston = data.frame(Boston)

head(Boston)
names(Boston)
str(Boston)

##########################################################
#Let's see if there are any NA values in the dataset. 
##########################################################
sum(is.na(Boston))
#There seems to be no NA values. 

summary(Boston)
str(Boston)

#It looks like there are some categorical variables which are labelled numeric. Let's convert them. 
Boston$chas = as.factor(as.character(Boston$chas))
Boston$rad = as.factor(as.character(Boston$rad))
summary(Boston)

#Let's do outlier detection and removal. 
par(mfrow=c(1,2)) 
plot(density(Boston$zn), main = "Density of Zn.")
boxplot(Boston$zn, main = "Boxplot of Zn.")
#Looks like there are a lot of outliers with a heavily skewed right tail.
outliers_zn = sort(boxplot.stats(Boston$zn)$out) #values greater than 33 have to be taken out. 
rm.index = which(Boston$zn >= 33.0)
Boston = Boston[-rm.index,]; rm(outliers_zn)

par(mfrow=c(1,2)) 
plot(density(Boston$indus), main = "Density of Indus.")
boxplot(Boston$indus, main = "Boxplot of Indus.") #Bi-modal data with no outliers. 

par(mfrow=c(1,2)) 
plot(density(Boston$nox), main = "Density of Nox.")
boxplot(Boston$nox, main = "Boxplot of Nox.")


par(mfrow=c(1,2)) 
plot(density(Boston$rm), main = "Density of Rm.")
boxplot(Boston$rm, main = "Boxplot of Rm.")
#Looks like there are a lot of outliers on the either side of whiskers. 
outliers_rm = sort(boxplot.stats(Boston$rm)$out) #Looking at the density curve it's evident that 
#values less than 4.926 and the values that are greater than 7.393 are outliers. 
rm.index = which(Boston$rm <= 4.926)
rm.index = append(rm.index, which(Boston$rm >= 7.393))
Boston = Boston[-rm.index,]

par(mfrow=c(1,2)) 
plot(density(Boston$age), main = "Density of Age.")
boxplot(Boston$age, main = "Boxplot of Age.") #left tailed without outliers. 

par(mfrow=c(1,2)) 
plot(density(Boston$dis), main = "Density of Dis.")
boxplot(Boston$dis, main = "Boxplot of Dis.") #Right tailed with a couple of outliers. 
outliers_rm = sort(boxplot.stats(Boston$dis)$out) 
rm.index = which(Boston$dis >= 8.90)
Boston = Boston[-rm.index,]


par(mfrow=c(1,2)) 
plot(density(Boston$tax), main = "Density of Tax.")
boxplot(Boston$tax, main = "Boxplot of Tax.") #Bi-modal data without outliers.


par(mfrow=c(1,2)) 
plot(density(Boston$ptratio), main = "Density of Ptratio.")
boxplot(Boston$ptratio, main = "Boxplot of Ptratio.") #Left tailed with outlier.
outliers_rm = sort(boxplot.stats(Boston$ptratio)$out) 
rm.index = which(Boston$ptratio == 13)
Boston = Boston[-rm.index,]


par(mfrow=c(1,2)) 
plot(density(Boston$black), main = "Density of Black.")
boxplot(Boston$black, main = "Boxplot of Black.") #Heavy left tailed with many outliers. 
outliers_rm = sort(boxplot.stats(Boston$black)$out) 
rm.index = which(Boston$black <= 331.3)
Boston = Boston[-rm.index,]


par(mfrow=c(1,2)) 
plot(density(Boston$lstat), main = "Density of Lstat.")
boxplot(Boston$lstat, main = "Boxplot of Lstat.") #Right tailed with some outliers. 
outliers_rm = sort(boxplot.stats(Boston$lstat)$out) 
rm.index = which(Boston$lstat > 29.5)
Boston = Boston[-rm.index,]


par(mfrow=c(1,2)) 
plot(density(Boston$medv), main = "Density of Medv.")
boxplot(Boston$medv, main = "Boxplot of Medv.") #Right tailed with some outliers. 
outliers_rm = sort(boxplot.stats(Boston$medv)$out) 
rm.index = which(Boston$medv <= 9.71)
rm.index = append(rm.index, which(Boston$medv >= 32.4))
Boston = Boston[-rm.index,]
rm(outliers_rm, rm.index)

#Let's do some plotting to look at some correspondance between features. 
library(ggplot2)
library(GGally)
ggpairs(Boston)
#From the pairplots above. Most of the data plotted looks like blobs of data. 
#Except for medv,lstat and dis,nox, rm,lstat and rm,medv. Let's plot them seperately for a cleaner look. 

ggplot(Boston, aes(x = medv, y = lstat)) + geom_point() + labs(title = "Interaction between Medv and Lstat.")
#There seems to be a downward trend between price of homes and % of lower status population. 
#The relationship makes sense!


ggplot(Boston, aes(x = dis, y = nox)) + geom_point() + labs(title = "Interaction between Dis and Nox.")
#The relationship makes sense. The closer you are to the employment center, more is Nitric Oxide concentration (Pollution).

ggplot(Boston, aes(x = rm, y = lstat)) + geom_point() + labs(title = "Interaction between Rm and Lstat.")
#The relationship makese sense. As the % of lower population increases no of rooms decrease.

ggplot(Boston, aes(x = rm, y = medv)) + geom_point() + labs(title = "Interaction between Rm and Medv.")
#As the no of rooms increase the price of the house also increases. 


###
#b
###
#From the pairplots, there doesn't seem to be any relationship.
#Let's do a regression to see the relationship between per capita crime rate and others. 
lm_model = lm(crim~., Boston)
summary(lm_model)
#Features rm, age, dis, rad24, rad7, ptration and medv seem to be important. 


####
#c
####
#high crime
plot(density(Boston$crim), main = "Density of Crime.")
inspect_data = head(Boston[order(-Boston$crim),], 20) #lets take the highest 20 observations where there is maximum crime. 
summary(inspect_data)
#observations:
# zn = 0, indus = 18.1, chas = 0, rad = 24 (that is more pollution), ptratio = 20.2


#high taxrates. 
plot(density(Boston$tax), main = "Density of Tax.")
inspect_data = head(Boston[order(-Boston$tax),], 20) #lets take the highest 20 observations where there is maximum crime. 
summary(inspect_data)
#more age(mean = 93), zn = 0, many chas = 0 (16), more rad (24 is 17 times)


#high ptratio
plot(density(Boston$ptratio), main = "Density of Ptraio.")
inspect_data = head(Boston[order(-Boston$ptratio),], 20) #lets take the highest 20 observations where there is maximum crime. 
summary(inspect_data)
# zn = 0, chas =0, rad = 4, tax = 666, ptratio = 20.2. 


####
#d
####
length(which(Boston$rm >= 7))
#6


inspect_data = Boston[which(Boston$rm >= 8),]
#There are no observations that are greater than 8. May be I might have taken them out as outliers. 
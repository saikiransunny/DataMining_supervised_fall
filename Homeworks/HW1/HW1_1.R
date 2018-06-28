########################
#Clean the work station.
########################
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW1")
######
#Q1
######
#####
library(ISLR)
data("Auto")
head(Auto)
names(Auto)
str(Auto)

##########################################################
#Let's see if there are any NA values in the dataset. 
##########################################################
sum(is.na(Auto))


##############################################################################################
#Looks like there are some features which are supposed to be categorical, but are numeric. 
#Hence we need to convert them. 
#############################################################################################
Auto$cylinders = as.factor(as.character(Auto$cylinders))
Auto$year = as.factor(as.character(Auto$year))
Auto$origin = as.factor(as.character(Auto$origin))
summary(Auto)

#From the first glance, it looks like name has a lot of features and it is actually categorical. 
#So if we run regression on it we are going to have 370 new features. 
#Instead of deleting it, let's try to create a new column with manufacture's name. 

Auto$company_name = sapply(Auto$name, function(x) unlist(strsplit(as.character(x), " "))[1])
table(Auto$company_name)
#remove the complete name of car. 
Auto = subset(Auto, select = -c(name))

#Looks like there are some spelling errors. 
correct_companynames = function(Auto){
Auto$company_name = gsub(pattern = "chevroelt|chevy", replacement = "chevrolet", x = Auto$company_name)
Auto$company_name = gsub(pattern = "maxda", replacement = "mazda", x = Auto$company_name)
Auto$company_name = gsub(pattern = "mercedes-benz", replacement = "mercedes", x = Auto$company_name)
Auto$company_name = gsub(pattern = "toyouta", replacement = "toyota", x = Auto$company_name)
Auto$company_name = gsub(pattern = "vw|vokswagen", replacement = "volkswagen", x = Auto$company_name)

#After some googling, there is no 'hi 1200d'. There is a 'Honda 1200d'. Hence changing.  
Auto$company_name = gsub(pattern = "hi", replacement = "honda", x = Auto$company_name)
return(Auto)
}
Auto = correct_companynames(Auto)
table(Auto$company_name)
Auto$company_name = as.factor(as.character(Auto$company_name))

#######################################################
#Are there any outliers? If yes let's deal with them.
#######################################################
######
summary(Auto)
#In the feature cylinders, for values 3,5 there seems to be quite little data. 
#Though it is little I choose to keep them since I don't see any other way that I can replace them. 
#Like we did with the names. 

par(mfrow=c(1,2)) 
plot(density(Auto$displacement), main = "Density of Displacement.")
boxplot(Auto$displacement, main = "Boxplot of Displacement.")



par(mfrow=c(1,2)) 
plot(density(Auto$horsepower), main = "Density of Horsepower.")
boxplot(Auto$horsepower, main = "Boxplot of Horsepower.")
#Looks like there are some outliers in Horsepower. Let's remove those. 
#First let's see clearly the distribution of Horsepower. 
summary(Auto$horsepower) #Looks legit. The third quartile is 126 and the max is 230. That is 80% more!
#Removing the outliers. 
outliers = sort(boxplot.stats(Auto$horsepower)$out)
Auto = Auto[-which(Auto$horsepower >= 208),]
rm(outliers)

par(mfrow=c(1,2)) 
plot(density(Auto$weight), main = "Density of Weight.")
boxplot(Auto$displacement, main = "Boxplot of Weight.")


par(mfrow=c(1,2)) 
plot(density(Auto$acceleration), main = "Density of Acceleration.")
boxplot(Auto$acceleration, main = "Boxplot of Acceleration.")
#Looks like there are some outliers in Acceleration. Let's remove those. 
#Like before let's look at the distribution. 
summary(Auto$acceleration)
#It is unclear which values to remove in the upper whisker. So, let's do..
outliers_acceleration = sort(boxplot.stats(Auto$acceleration)$out)
outliers_acceleration
#Let's remove the values that are (less than or equal to 8.5) and (greater than or equal to 22.1)
Auto = Auto[-which(Auto$acceleration <= 8.5),]
Auto = Auto[-which(Auto$acceleration >= 22.1),]
#####
rm(outliers_acceleration)


#############################################################################
#Let us do some exploratory data analysis now that the outliers are removed. 
#############################################################################
#pair plot. 
library(GGally)
ggpairs(Auto[,1:8]) #Let's not include company name. Since there are so many.


#############################################################################
#Is there a trend for year, displacement, horsepower, weight, acceleration?
#############################################################################
library(ggplot2)
plot_data = aggregate(acceleration ~ year, Auto, mean)
ggplot(data = plot_data, aes(x = year, y = acceleration)) + 
  geom_point()+
  labs(title = "Relationship between Year and Mean-Acceleration")
#Looks like there is clear upward trend between the year and acceleration. 
cor(as.numeric(as.character(plot_data$year)), plot_data$acceleration)
#Infact there is a 76% correlation between the year(converting into numeric) and acceleration. 

####################################################
#Is there a trend between year and mean-horsepower?
####################################################
plot_data = aggregate(horsepower ~ year, Auto, mean)
ggplot(data = plot_data, aes(x = year, y = horsepower)) + 
  geom_point() + labs(title = "Relationship between Year and Mean-HorsePower")
#Clear downward trend can be seen with correlation -84%
cor(as.numeric(as.character(plot_data$year)), plot_data$horsepower)

############################################
#Is there a trend for year and mean-weight?
############################################
plot_data = aggregate(weight ~ year, Auto, mean)
ggplot(data = plot_data, aes(x = year, y = weight)) + 
  geom_point() + labs(title = "Relationship between Year and Mean-weight")
cor(as.numeric(as.character(plot_data$year)), plot_data$weight)
#Clear downward trend can be seen with correlation -77%
#It makes sense why the acceleration increased. It could probably have been due to decrease in weight over time!

########################################################################################
#It makes intuitive sense that with more number of cylinders, more will be horsepower 
# and thus accelleration. Let's check what the data says!
########################################################################################
plot_data = aggregate(horsepower~cylinders, Auto,mean)
ggplot(data = plot_data, aes(x = cylinders, y = horsepower)) + 
  geom_point() + labs(title = "Relationship between Cylinders and Mean-HorsePower")
#Looks like with respect to getting horsepower out of the engine, 4,5 design doesn't comply.
#But the general trend is what we expected. That is horsepower increases with increase in cylinders.

plot_data = aggregate(acceleration~cylinders, Auto, mean)
ggplot(data = plot_data, aes(x = cylinders, y = acceleration)) + 
  geom_point() + labs(title = "Relationship between Cylinders and Mean-Acceleration")
#But interestingly 5 cylinder engine has great acceleration. Looks like it is the tradeoff 
#engineers have to make with acceleration and horsepower while designing an engine for a car.



###############################################################################################
#Now let's see some relationships with the target variable. 
#We can get the inferences from the pairplot we plotted. Let's look at other relationships
###############################################################################################
plot_data = aggregate(mpg~cylinders, Auto, mean)
ggplot(data = plot_data, aes(x = cylinders, y = mpg)) + 
  geom_point() + labs(title = "Relationship between Cylinders and MPG")
#Like one can expect the mileage goes down with more cylinders (sports cars!)

#MPG and year
plot_data = aggregate(mpg~year, Auto, mean)
ggplot(data = plot_data, aes(x = year, y = mpg)) + 
  geom_point() + labs(title = "Relationship between Year and MPG")
#Looks like technology has improved over the years!


#MPG and origin
plot_data = aggregate(mpg~origin, Auto, mean)
ggplot(data = plot_data, aes(x = origin, y = mpg)) + 
  geom_point() + labs(title = "Relationship between Origin and MPG")
#clearly increasing!


#MPG and company_name
plot_data = aggregate(mpg~company_name, Auto, mean)
plot_data = plot_data[order(plot_data$mpg),]
plot_data$company_name[1] # Chrysler has the lowest Mileage!
plot_data$company_name[nrow(plot_data)] # Nissan has the highest mileage!
ggplot(data = plot_data, aes(x = company_name, y = mpg)) + 
  geom_point() + labs(title = "Relationship between Company_Name and MPG")

rm(plot_data)


#####
#Q2
#####
#a and b
rm(list = ls(all = T))
setwd("/home/saikiran/Desktop/Fall 2017/applied_stats_rachael/Homeworks/HW1")
load("workspace/After_outlierremoval.RData")
linear_model = lm(mpg~., Auto)
summary(linear_model)
#a) Features, Cylinders4, Cylinders5, Cylinders6, Cylinders8, horsepower, weight, acceleration, 
#year77, year78, year79, year80, year81, year82, company_namebuick, company_namecadillac,company_nameoldsmobile, 
#company_nameplymouth, company_namepontiac are important. 

#b) While year71-year76 seem not significant. Years 77-82 are important. While others have a very low coefficient
#77-82 have high coefficient. For example for 82, the coefficient is 6.9. This means for unit increase (basically yes or no since it is a factor)
#MPG increases by 6.9. Like we saw in graphs, as the years progressed MPG increased drastically. 

#2-c
# *, : are alternate ways to do accomplish the same thing. That is two way interaction between the features. 
#Let's look only at the numeric two way interaction for simplicity. 
interaction_linear_model = lm(mpg~.+displacement*horsepower+displacement*weight+displacement*acceleration+horsepower*weight+horsepower*acceleration+weight*acceleration, Auto) 
summary(interaction_linear_model)
#In the two way interaction apart from the ones that are mentioned without interaction,
#[displacement,weight] and [horsepower,acceleration] are found to be important.



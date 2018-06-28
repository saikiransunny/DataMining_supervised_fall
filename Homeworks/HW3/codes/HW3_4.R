rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/Homeworks/HW3/codes")

set.seed(1)
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)

library(boot)
data = data.frame(x, y)

for(i in 1:4){
glm.fit = glm(y~poly(x,i))
print(paste(i, ":", cv.glm(data, glm.fit)$delta, sep = ""))
}
#error for 2 is the lowest. 

summary(glm.fit)

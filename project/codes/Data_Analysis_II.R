rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/project")

train = read.csv("data/after_outlierandtrasformation/train.csv")
test = read.csv("data/after_outlierandtrasformation/test.csv")


#creating new features. 
#length of name
library(stringr)
train$length_Name = sapply(train$Name, function(x) str_count(as.character(x), '\\w+'))
test$length_Name = sapply(test$Name, function(x) str_count(as.character(x), '\\w+'))


#length of publisher. 
train$length_Publisher = sapply(train$Publisher, function(x) str_count(as.character(x), '\\w+'))
test$length_Publisher = sapply(test$Publisher, function(x) str_count(as.character(x), '\\w+'))

#platform is highly cardinal. Decreasing the cardinality. 
table(train$Platform)
train$new_platform = train$Platform
test$new_platform = test$Platform
train = subset(train, select = -c(Platform))
test = subset(test, select = -c(Platform))
change_platform = function(dataframe){
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "X360|XB|XOne", replacement = "XB", x))
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "Wii|WiiU", replacement = "Wii", x))
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "PS|PS2|PS3|PS4|PSP|PSV", replacement = "PS", x))
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "DS|3DS|SNES|N64|NES", replacement = "DS", x))
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "GB|GBA", replacement = "GB", x))
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "3DO|GEN|GG|NG|SAT|DC|SCD|TG16|WS", replacement = "others", x))
  dataframe$new_platform = sapply(dataframe$new_platform, function(x) gsub(pattern = "PC|PCFX", replacement = "PC", x))
  
return(dataframe)
}
train = change_platform(train)
table(train$new_platform)
test = change_platform(test)
train$new_platform = as.factor(as.character(train$new_platform))
test$new_platform = as.factor(as.character(test$new_platform))
#check if they are consistent in train and test. 
sort(unique(train$new_platform))
sort(unique(test$new_platform))



#variations in year. 
#5 year periods 
train$yearI = as.numeric(as.character(train$Year_of_Release))
train$yearI = sapply(train$yearI, function(x) ifelse(x %in% seq(1980, 1985), 0, 
                                                     ifelse(x %in% seq(1986, 1990), 1, 
                                                            ifelse(x %in% seq(1991,1995), 2, 
                                                                   ifelse(x %in% seq(1996,2000),3,
                                                                          ifelse(x %in% seq(2001,2005),4,
                                                                                 ifelse(x %in% seq(2006,2010),5,
                                                                                        ifelse(x %in% seq(2011,2015), 6,
                                                                                               ifelse(x %in% seq(2016,2020),7,999)))))))))
test$yearI = as.numeric(as.character(test$Year_of_Release))
test$yearI = sapply(test$yearI, function(x) ifelse(x %in% seq(1980, 1985), 0, 
                                                    ifelse(x %in% seq(1986, 1990), 1, 
                                                           ifelse(x %in% seq(1991,1995), 2, 
                                                                  ifelse(x %in% seq(1996,2000),3,
                                                                         ifelse(x %in% seq(2001,2005),4,
                                                                                ifelse(x %in% seq(2006,2010),5,
                                                                                       ifelse(x %in% seq(2011,2015), 6,
                                                                                              ifelse(x %in% seq(2016,2020),7,999)))))))))
train$yearI = as.factor(as.character(train$yearI))
test$yearI = as.factor(as.character(test$yearI))
#10 year periods
train$yearII = as.numeric(as.character(train$Year_of_Release))
train$yearII = sapply(train$yearII, function(x) ifelse(x %in% seq(1980, 1990), 0, 
                                                       ifelse(x %in% seq(1991, 2000), 1, 
                                                              ifelse(x %in% seq(2001, 2010), 2,
                                                                     ifelse(x %in% seq(2011, 2020), 3, 999)))))
train$yearII = as.factor(as.character(train$yearII))

test$yearII = as.numeric(as.character(test$Year_of_Release))
test$yearII = sapply(test$yearII, function(x) ifelse(x %in% seq(1980, 1990), 0, 
                                                      ifelse(x %in% seq(1991, 2000), 1, 
                                                             ifelse(x %in% seq(2001, 2010), 2,
                                                                    ifelse(x %in% seq(2011, 2020), 3, 999)))))
test$yearII = as.factor(as.character(test$yearII))
#check consistency in yearI and YearII in train and test. 
sort(unique(train$yearI))
sort(unique(test$yearI))

sort(unique(train$yearII))
sort(unique(test$yearII))



#normal plots. 
######
library(dplyr)
library(ggplot2)
inspect = train %>% count(yearII)
ggplot(data=inspect, aes(x = yearII, y = n)) +
  geom_bar(stat="identity") + ggtitle("Variation in YearII") + 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearII") + ylab("Count")


inspect = train %>% count(yearI)
ggplot(data=inspect, aes(x = yearI, y = n)) +
  geom_bar(stat="identity")+ ggtitle("YearI")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearI") + ylab("Count")


inspect = train %>% count(new_platform)
ggplot(data=inspect, aes(x = new_platform, y = n)) +
  geom_bar(stat="identity")+ ggtitle("new_platform")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("new_platform") + ylab("Count")


inspect = train %>% count(Genre)
ggplot(data=inspect, aes(x = Genre, y = n)) +
  geom_bar(stat="identity")+ ggtitle("Genre")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Genre") + ylab("Count")


inspect = train %>% count(Rating)
ggplot(data=inspect, aes(x = Rating, y = n)) +
  geom_bar(stat="identity")+ ggtitle("Rating")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Rating") + ylab("Count")
######


#pair plot. 
# library(GGally)
# ggpairs(train, columns = 5:17)



#grouping analysis. 
inspect = train %>% group_by(yearI) %>%
  summarise(mean_global_sales = median(Global_Sales,na.rm = T))
ggplot(data=inspect, aes(x = yearI, y = mean_global_sales)) +
  geom_bar(stat="identity")+ggtitle("Median sales grouped by YearI")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearI") + ylab("Median sales")

inspect = train %>% group_by(yearI) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T))
ggplot(data=inspect, aes(x = yearI, y = mean_critic_score)) +
  geom_bar(stat="identity")+ggtitle("Median critic_score grouped by YearI")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearI") + ylab("Median critic_score")

inspect = train %>% group_by(yearI) %>%
  summarise(mean_user_score = median(User_Score,na.rm = T))
ggplot(data=inspect, aes(x = yearI, y = mean_user_score)) +
  geom_bar(stat="identity")+ggtitle("Median user_score grouped by YearI")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearI") + ylab("Median user_score")

inspect = train %>% group_by(yearI) %>%
  summarise(mean_length_name = median(length_Name,na.rm = T))
ggplot(data=inspect, aes(x = yearI, y = mean_length_name)) +
  geom_bar(stat="identity")+ggtitle("Median length_game_name grouped by YearI")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearI") + ylab("Median length(game_name)")
#interestingly the length of names seem to have increased with time. 


#grouping by year2
inspect = train %>% group_by(yearII) %>%
  summarise(mean_global_sales = median(Global_Sales,na.rm = T))
ggplot(data=inspect, aes(x = yearII, y = mean_global_sales)) +
  geom_bar(stat="identity")+ggtitle("Median global_sales grouped by YearII")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearII") + ylab("Median global_sales")

inspect = train %>% group_by(yearII) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T))
ggplot(data=inspect, aes(x = yearII, y = mean_critic_score)) +
  geom_bar(stat="identity")+ggtitle("Median critic_score grouped by YearII")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearII") + ylab("Median critic_score")


inspect = train %>% group_by(yearII) %>%
  summarise(mean_user_score = median(User_Score,na.rm = T))
ggplot(data=inspect, aes(x = yearII, y = mean_user_score)) +
  geom_bar(stat="identity")+ggtitle("Median user_score grouped by YearII")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearII") + ylab("Median user_score")


inspect = train %>% group_by(yearII) %>%
  summarise(mean_length_name = median(length_Name,na.rm = T))
ggplot(data=inspect, aes(x = yearII, y = mean_length_name)) +
  geom_bar(stat="identity")+ggtitle("Median game_length grouped by YearII")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearII") + ylab("Median game_length")




#group by genre
inspect = train %>% group_by(Genre) %>%
  summarise(mean_global_sales = median(Global_Sales,na.rm = T))
ggplot(data=inspect, aes(x = Genre, y = mean_global_sales)) +
  geom_bar(stat="identity")+ggtitle("Median global_sales grouped by Genre")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Genre") + ylab("Median global_sales")

#interesting. 
#creating a new feature around it. 
train$is_important_genre = sapply(train$Genre, function(x) ifelse(x %in% c("Action", "Fighting", "Platform", "Shooter", "Sports"), 1, 0))
test$is_important_genre = sapply(test$Genre, function(x) ifelse(x %in% c("Action", "Fighting", "Platform", "Shooter", "Sports"), 1, 0))
train$is_important_genre = as.factor(as.character(train$is_important_genre))
test$is_important_genre = as.factor(as.character(test$is_important_genre))


inspect = train %>% group_by(Genre) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T))
ggplot(data=inspect, aes(x = Genre, y = mean_critic_score)) +
  geom_bar(stat="identity")+ggtitle("Median critic_score grouped by Genre")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Genre") + ylab("Median critic_score")


inspect = train %>% group_by(Genre) %>%
  summarise(mean_user_score = median(User_Score,na.rm = T))
ggplot(data=inspect, aes(x = Genre, y = mean_user_score)) +
  geom_bar(stat="identity")+ggtitle("Median user_score grouped by Genre")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Genre") + ylab("Median user_score")


inspect = train %>% group_by(Genre) %>%
  summarise(mean_length_name = median(length_Name,na.rm = T))
ggplot(data=inspect, aes(x = Genre, y = mean_length_name)) +
  geom_bar(stat="identity")+ggtitle("Median game_length grouped by Genre")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Genre") + ylab("Median game_length")




#group  by rating. 
inspect = train %>% group_by(Rating) %>%
  summarise(mean_global_sales = median(Global_Sales,na.rm = T))
ggplot(data=inspect, aes(x = Rating, y = mean_global_sales)) +
  geom_bar(stat="identity")+ggtitle("Median global_sales grouped by Rating")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Rating") + ylab("Median global_sales")
#interesting!

inspect = train %>% group_by(Rating) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T))
ggplot(data=inspect, aes(x = Rating, y = mean_critic_score)) +
  geom_bar(stat="identity")+ggtitle("Median critic_score grouped by Rating")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Rating") + ylab("Median critic_score")


inspect = train %>% group_by(Rating) %>%
  summarise(mean_user_score = median(User_Score,na.rm = T))
ggplot(data=inspect, aes(x = Rating, y = mean_user_score)) +
  geom_bar(stat="identity")+ggtitle("Median user_score grouped by Rating")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Rating") + ylab("Median user_score")


inspect = train %>% group_by(Rating) %>%
  summarise(mean_length_name = median(length_Name,na.rm = T))
ggplot(data=inspect, aes(x = Rating, y = mean_length_name)) +
  geom_bar(stat="identity")+ggtitle("Median game_length grouped by Rating")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("YearII") + ylab("Median game_length")





#group by new platform. 
inspect = train %>% group_by(new_platform) %>%
  summarise(mean_global_sales = median(Global_Sales,na.rm = T))
ggplot(data=inspect, aes(x = new_platform, y = mean_global_sales)) +
  geom_bar(stat="identity")+ggtitle("Median global_sales grouped by Platform")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Platform") + ylab("Median global_sales")

#intersting
#creating a new feature around it. 
train$is_important_platform = sapply(train$new_platform, function(x) ifelse(x %in% c("2600", "GB", "XB", "PS", "Wii"), 1, 0))
test$is_important_platform = sapply(test$new_platform, function(x) ifelse(x %in% c("2600", "GB", "XB", "PS", "Wii"), 1, 0))
train$is_important_platform = as.factor(as.character(train$is_important_platform))
test$is_important_platform = as.factor(as.character(test$is_important_platform))


inspect = train %>% group_by(new_platform) %>%
  summarise(mean_critic_score = median(Critic_Score,na.rm = T))
ggplot(data=inspect, aes(x = new_platform, y = mean_critic_score)) +
  geom_bar(stat="identity")+ggtitle("Median critic_score grouped by platform")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("platform") + ylab("Median critic_score")


inspect = train %>% group_by(new_platform) %>%
  summarise(mean_user_score = median(User_Score,na.rm = T))
ggplot(data=inspect, aes(x = new_platform, y = mean_user_score)) +
  geom_bar(stat="identity")+ggtitle("Median user_score grouped by platform")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("platform") + ylab("Median user_score")


inspect = train %>% group_by(new_platform) %>%
  summarise(mean_length_name = median(length_Name,na.rm = T))
ggplot(data=inspect, aes(x = new_platform, y = mean_length_name)) +
  geom_bar(stat="identity")+ggtitle("Median game_length grouped by platform")+ 
  theme(plot.title = element_text(hjust = 0.5)) + xlab("platform") + ylab("Median game_length")


write.csv(train, "data/after_humanfeaturegen/train.csv", row.names = F)
write.csv(test, "data/after_humanfeaturegen/test.csv", row.names = F)
  
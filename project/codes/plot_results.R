rm(list = ls(all = T))
setwd("/home/saikiran/Fall 2017/applied_stats_rachael/project")


human_features = read.csv("results/results_humanfeatures.csv")
NN_features = read.csv("results/results_neuralnetfeautres.csv")


names(human_features) = c("Model", "RMSE")
ggplot(data=human_features, aes(x=Model, y=RMSE, group=1)) +
  geom_line() + geom_point() + ggtitle("Models performance on Original Features") +
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=18))


names(NN_features) = c("Model", "RMSE")
ggplot(data=NN_features, aes(x=Model, y=RMSE, group=1)) +
  geom_line() + geom_point() + ggtitle("Models performance on NN Features") +
  theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=18))


Merged_results = merge(NN_features, human_features, by.x = "model", by.y = "model_name")
names(Merged_results) = c("Model", "NN_features", "Human_features")


p = plot_ly(Merged_results, x = ~Model, y = ~NN_features, name = 'NN_features', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~Human_features, name = 'Human_features', mode = 'lines+markers') %>%
  layout(title = "Performance of NNfeatures and HumanFeatures",
       xaxis = list(title = "Models"),
       yaxis = list (title = "RMSE"))
p

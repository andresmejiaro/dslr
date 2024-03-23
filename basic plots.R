library(ggplot2)
library(GGally)

magic=read.csv("datasets/dataset_train.csv")


ggpairs(magic,columns =7:19, ggplot2::aes(color=magic$Hogwarts.House))

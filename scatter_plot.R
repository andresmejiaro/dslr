#! /usr/bin/Rscript

library(ggplot2)
library(patchwork)


main = function(x){
  options(warn = -1)
  magic=read.csv("datasets/dataset_train.csv")
  p1 = ggplot(data = magic, aes(x = Astronomy, 
    y= Defense.Against.the.Dark.Arts,
      color = Hogwarts.House ) ) + 
    geom_point() + 
     labs(color = "House", y = "Defense Against the Dark Arts")
  ggsave("scatter_plot.png", p1)
  system("xdg-open scatter_plot.png")
}

main()
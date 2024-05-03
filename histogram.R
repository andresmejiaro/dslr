#! /usr/bin/Rscript

library(ggplot2)
library(patchwork)


main = function(x){
    options(warn = -1)
    magic=read.csv("datasets/dataset_train.csv")
    p1 = ggplot(data = magic, aes(x = Arithmancy, fill = Hogwarts.House ) ) + 
      geom_histogram(bins = 26, alpha = 0.5, position = "identity") + 
      geom_density() + labs(y= "Count", fill = "House")
    p2 = ggplot(data = magic, aes(x = Arithmancy, color = Hogwarts.House ) )  +
      geom_density( position = "identity") + labs(color = "House") 
    p3 = ggplot(data = magic, aes(x = Care.of.Magical.Creatures , 
      fill = Hogwarts.House ) ) + 
        geom_histogram(bins = 25, alpha = 0.5, position = "identity") + 
        geom_density()+ labs(y= "Count", fill = "House", x = "Care of Magical Creatures")
    p4 = ggplot(data = magic, aes(x = Care.of.Magical.Creatures , color = Hogwarts.House ) )  +
    geom_density( position = "identity") + labs(y= "Count", color = "House", x = "Care of Magical Creatures")
    p5 = p1 + p2 + p3 + p4
    ggsave("hist.png", p5)
    system("xdg-open hist.png")
}

main()
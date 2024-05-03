#! /usr/bin/Rscript

suppressPackageStartupMessages({library(GGally, warn.conflicts = F,quietly = T)})

main = function(x){
  options(warn = -1)
  magic=read.csv("datasets/dataset_train.csv")
  magic$Birthmonth = magic$Birthday %>% as.Date() %>% format.Date("%m") %>% as.numeric()
  magic$Birthdayofmonth = magic$Birthday %>% as.Date() %>% format.Date("%d") %>% as.numeric()
  magic$daten = magic$Birthday %>% as.Date()
  p1 = ggpairs(magic,columns =7:22, ggplot2::aes(color=magic$Hogwarts.House))
  ggsave("pair_plot.png", p1, width = 20, height = 15, dpi = 300)
  if (Sys.info()["sysname"] == "Darwin") {
    system("open pair_plot.png")
  } else if (Sys.info()["sysname"] == "Linux") {
    system("xdg-open pair_plot.png")
  }
}

main()
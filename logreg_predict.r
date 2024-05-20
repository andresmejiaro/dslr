source("logistic_regression.R")
library(magrittr, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(purrr, warn.conflicts = F)
library(optparse, warn.conflicts = F)


predict_house = function(newData, weights){
  HouseOrder = weights$House 
  weights$House = NULL
  result = (as.matrix(newData)) %*% t(as.matrix(weights))
  result2 = t(result) %>% as.data.frame() %>% 
    map_chr(function(x) HouseOrder[which.max(x)])
  exit = data.frame(Index = 0:(length(result2)-1),"Hogwarts House"=result2)
  colnames(exit) = c("Index","Hogwarts House")
  exit
}



main = function(){
  
  c_args = commandArgs(trailingOnly = T)
  if (length(c_args) != 1){
    print("Wrong Number of Args")
    q()
  }
  testdb = load_database(c_args[1])
  if (!file.exists("weights.csv") ||
      !file.exists("normalization_constants.csv")){
    cat("Can't find training files. Make sure to train your model first\n")
    q()
  }
  weights = load_database("weights.csv")
  weights$X = NULL
  testdb %<>% select(any_of(names(weights))) 
  normalizing_constants = load_database("normalization_constants.csv")
  testdb = df_prepros(testdb, normalizing_constants)
  testdb$ones = 1
  exit = predict_house(testdb,weights)
  tryCatch(write.csv(exit,"houses.csv",row.names=F,quote = F),
           error = function (e){
             cat("Something went wrong writing the prediction\n")
             q()
           } )
  
  cat("Writing prediction to houses.csv\n")
}




main()


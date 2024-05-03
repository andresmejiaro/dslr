#! /usr/bin/Rscript

source("summarize.R")
source("logistic_regression.R")
library(optparse)



main = function(){
  option_list = list(
    make_option(c("--nbatch"), type="integer", default=1, 
                help="Specifies the number of batches to use [default= %default]", 
                metavar="METHOD"),
    make_option(c("--stochastic"), type="logical", default=F, 
                help="Sets the number of batches to the number of observations. Overwrites --nbatch", 
                metavar="METHOD"),
    make_option(c("--momentum"), type="character", default="GD", 
                help="Sets the the momentum method to use.", 
                metavar="METHOD")
  )
  parser = OptionParser(option_list=option_list, 
                        usage="%prog [options] file",
                        add_help_option=TRUE)
  args = parse_args(parser,positional_arguments = T)
  if (length(args$args) != 1){
    cat("Wrong Number of Args\n")
    q()
  }
  df = load_database(args$args[1])  
  if (!"Hogwarts.House" %in% colnames(df)){
    cat("Training Dataframe does not contain a valid target.")
  }
  target = df$Hogwarts.House
  df = df_prepros(df)
  nbatch = args$options$nbatch
  if (args$options$stochastic){
    nbatch = dim(df)[1]
  }
  if (!args$options$momentum %in% c("GD", "MO")){
    cat("Momentum Argument unknown\n")
    q()
  }


  weights = one_vs_all(df, target,nbatch, args$options$momentum)
  tryCatch(write.csv(weights, "weights.csv",row.names=F,quote = F),
           error = function (e){
             cat("Something went wrong writing the prediction\n")
             q()
           } )
  
  
}


main()

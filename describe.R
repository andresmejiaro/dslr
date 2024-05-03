#! /usr/bin/Rscript

source("summarize.R")
main = function(){
  args = commandArgs(trailingOnly = T)
  if (length(args) != 1){
    print("Wrong Number of Args")
    q()
  }
  else{
    tryCatch({
      df = read.csv(args[1])
    }, error = function(e){
      print("An error occurred while opening the file")
      q()
    })
  }
  describedf(df)
  q()
}

main()


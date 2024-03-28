source("summarize.R")
source("logistic_regression.R")

load_database = function(file_name){
  tryCatch({
    df = read.csv(file_name)
  }, error = function(e){
    print("An error occurred while opening the file")
    q()
  })
  df
}


df_prepros = function(df){
  descr = df %>% describedf(pr = F) 
  descr = descr[c("Mean","Std"),]
  target = df$Hogwarts.House
  if (is.null(target)){
    print("Does not have a valid target")
    q()
  }
  df = df %>% select(names(descr))
  df_names = names(descr)
  df = map2(df,descr, prepros) %>% map(as.data.frame) %>% list_cbind()
  logreg(df,target = target=="Slytherin")
}


main = function(){
  c_args = commandArgs(trailingOnly = T)
  if (length(c_args) != 1){
    print("Wrong Number of Args")
    q()
  }
  df = load_database(c_args[1])  
  df = df_prepros(df)
  
}


main()

library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)

summarize_col = function(column,colna = "data"){
  col2 = sort(column)
  col2 = Filter(function(x) !is.na(x), col2)
  res = list()
  res$or_count=Reduce(function(x,y) x + 1 , column,0)
  
  res$sq_sum=Reduce(function(x,y) x + y^2 , col2,0)
  res$sum=Reduce(function(x,y) x + y , col2,0)
  res$Count=Reduce(function(x,y) x + 1 , col2,0)
  res$sq_mean=res$sq_sum / res$Count
  res$Mean=res$sum/res$Count
  res$Var = res$sq_mean - res$Mean^2
  res$Std = res$Var^0.5
  res$Min = col2[1]
  res$Max = col2[res$Count]
  for( j in 1:3){
    aproxel= (res$Count + 1) * 0.25 *j
    aproxel_d = floor(aproxel)
    aproxel_u = ceiling(aproxel)
    w = col2[aproxel_d] * (aproxel_u-aproxel) + 
      col2[aproxel_u]*(aproxel - aproxel_d)
    if (j == 1){
      res$Q25 = w
    }
    if (j == 2){
      res$Q50 = w
    }
    if (j == 3){
      res$Q75 = w
    }
  }
  res = res[c("Count","Mean", "Std","Min","Q25","Q50","Q75","Max")]
  namesr= names(res)
  dfff = data.frame(list_c(res),row.names = namesr) 
  colnames(dfff)=colna
  dfff
}

describedf = function(df,pr=T){
  if(!is.data.frame(df)){
    stop("Non dataframe to describe")
  }
  df2 = df %>% select(where(is.numeric), -Index)
  
  df3 = map2(df2,df2 %>% colnames ,summarize_col) %>% list_cbind()
  if (pr){
    ncols = dim(df3)[2]
    sets=split(1:ncols,floor((1:ncols-1)/4))
    for(m in sets){
      print(df3[,m])
    }
  }
  write.csv(df3,"Normalization.csv")
  df3
}

#Normalize and impute missing values
prepros =  function(col,mean_sd){
  mean = mean_sd["Mean",1]
  sd = mean_sd["Std",1]
  col = (col -mean)/sd
  col[is.na(col)] = 0
  col
}

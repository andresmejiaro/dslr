sigm = function(x){
  1/(1 + exp(-x))
}


logreg = function(features, target){
  features$ones = 1
  betas = map(features, function(x) {0})
  alpha = 0.01
  while (T){
  h = map2(features,betas,function(x,y) x*y[1]) %>% 
    map(as.data.frame) %>% list_cbind() %>% rowSums() %>% 
    sigm
  cost = -1/dim(features)[1]* sum(target * log(h) +
                                (1-target)*log(1-h))
  print(cost)
  J = map(features, function(x){sum(x * (h - target))/dim(features)[1]}) 
  betas = map2(betas,J, function(x,y) x - alpha*y)
  
  
  }
  
}


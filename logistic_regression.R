sigm <- function(x) {
  1 / (1 + exp(-x))
}


logreg <- function(features, target, nslices, momentum = "GD") {
  features$ones <- 1
  betas <- map_dbl(features, function(x) {
    0
  })
  betaold = betas
  alpha <- 0.1 / nslices
  gamma =(momentum == "MO")*0.9

  last_cost <- Inf
  cost <- Inf
  out_break <- F
  for (w in 1:20000) {
    indices <- sample(rep(1:nslices, length.out = nrow(features)))
    for (k in 1:nslices) {
      h <- map2(features, betas, function(x, y) x * y[1]) %>%
        map(as.data.frame) %>%
        list_cbind() %>%
        rowSums() %>%
        sigm()
      cat(sprintf("\rCost: %.4f Epoch: %d Batch: %d", cost, w, k))
      
      J <- map_dbl(features[indices == k, ], function(x) {
        sum(x * (h[indices == k] -
          target[indices == k])) / sum(indices == k)
      })
      # betas <- map2(betas, J, function(x, y) x*(1+gamma) - alpha * y) %>% map2(betaold,
      #   function(x,y) x - gamma*y)
      betaold=betaold*gamma + J
      betas = betas - alpha * betaold
    }
    last_cost <- cost
    cost <- -1 / dim(features)[1] * sum(target * log(h) +
      (1 - target) * log(1 - h))
    if (!is.infinite(last_cost) && abs(last_cost / cost - 1) < 1E-06) {
      break
    }
  }
  cat(sprintf("\n"))
  betas
}

load_database <- function(file_name) {
  tryCatch(
    {
      df <- read.csv(file_name)
    },
    error = function(e) {
      print(paste("An error occurred while opening the file: ", filename))
      q()
    }
  )
  df
}


df_prepros <- function(df, descr = NULL) {
  if (is.null(descr)) {
    descr <- df %>% describedf(pr = F)
    tryCatch(write.csv(file = "normalization_constants.csv", descr, row.names = T),
      error = function(e) {
        cat("Something went wrong writing the normalization constants\n")
        q()
      }
    )
    descr$X <- row.names(descr)
    
  }
  descr %<>% filter(X %in% c("Mean", "Std"))
  df <- df %>% select(any_of(names(descr)))
  df_names <- names(descr)
  for (name in df_names) {
    if (name == "X") {
      next
    }
    df[name] <- (df[name] - descr[descr$X == "Mean", name]) / descr[descr$X == "Std", name]
  }
  df[is.na(df)] <- 0
  df
}

one_vs_all <- function(df, target, nslices, momentum) {
  targets <- unique(target)
  coefs <- map_dfr(targets, function(x) {
    print(x)
    logreg(df, target == x, nslices, momentum)
  })
  coefs$House <- targets
  coefs
}

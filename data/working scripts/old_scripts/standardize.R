## Standardize
# Normalization and centralization
# Caret package offers automatic standarization and centering.
# Thus, this function is superfluous. 
# I still save it. Who knows, if we still need it. 


for (name in names(data)){
  print(name)
  curr <- as.matrix(data[,name])
  print(class(curr))
  mu <- mean(curr)
  sigma <- sd(curr)
  maxx <- max(curr)
  minn <- min(curr)

  data[,name] <- (data[,name] - mu) / sigma # for standard score
  #data[,name] <- (data[,name] - min) / (maxx-min) # For min-max feature scaling.

}

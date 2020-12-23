require(MASS)
require(randomForest)

# Generates a covariance matrix with a given number of 'features' and a given
#   sample correlation 'sample.corr'
# NOTE: All features will have the same correlation with the others
# NOTE: All features have variance 1
covariance.matrix <- function(features, sample.corr){
  temp <- matrix(0, nrow = features, ncol = features)
  i.upr <- which(upper.tri(temp, diag = FALSE), arr.ind=TRUE)
  temp[i.upr] <- sample.corr
  l.upr <- which(lower.tri(temp, diag = FALSE), arr.ind=TRUE)
  temp[l.upr] <- sample.corr
  output <- temp + diag(x=1, nrow = features, ncol = features)
  return(output)
}

# Creates a set of 'n' observations for 'features' different normal random 
#   variables which have a given sample correlation 'sample.corr'. If
#   'empirical' is set to TRUE, the sample correlation will be exactly 
#   'sample.corr'
# NOTE: All features will have mean and variance 1
simulate.normal <- function(n, features, mu, sample.corr, empirical = TRUE){
  myCov <- covariance.matrix(features = features, sample.corr =  sample.corr)
  output <- mvrnorm(n = n, mu = mu, Sigma = myCov,
                    empirical = empirical)
  return(output)
}

# Changes the 'i' column from the list 'columns' from 'data' ordering it
#   using 'index'
shuffle.column <- function(i, data, columns, index){
  output <- data[ , columns[i] ]
  output <- output[index]
  return( output )
}

# Shuffles the features indicated by the vector 'columns' in 'data'
shuffle.features <- function(data, columns){
  index <- sample(1:nrow(data), replace = FALSE)
  for (i in 1:length(columns)) {
    temp <- shuffle.column(i, data = data, columns = columns, index = index)
    data[ , columns[i] ] <- temp
  }
  return(data)
}
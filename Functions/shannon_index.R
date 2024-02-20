#################################
# FUNCTION: shannon_index
# packages: vegan
# purpose: to get shannon diversity index
# input: df
# output: list
# -------------------------------
shannon_index <- function(x = NULL) {
  
  m <- matrix(ncol = 2, nrow = 1) 
  
  y <- diversity(x, index = "shannon")
  
  m[,1] <- mean(y)
  m[,2] <- sd(y)
  
  return(m)
  
}


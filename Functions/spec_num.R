#################################
# FUNCTION: spec_num
# packages: vegan
# purpose: calculate the number of species per site
# input: df
# output: species number mean and sd
# -------------------------------
spec_num <- function(x = NULL) {
  
  m <- matrix(ncol = 2, nrow = 1)
  
  z <- specnumber(x)
  
  m[,1] <- mean(z)
  m[,2] <- sd(z)
  
  return(m)
 
}


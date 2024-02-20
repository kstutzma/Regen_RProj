#################################
# FUNCTION: Pielous_J
# packages: vegan
# purpose: to calculate evenness
# input: df
# output: measure of evenness
# -------------------------------
Pielous_J <- function(x = NULL) {
  
  m <- matrix(ncol = 2, nrow = 1)
  
  H <- diversity(x)
  S <- specnumber(x)
  J <- H/(log(S))
  
  J1 <- na.omit(J)
  
  m[,1] <- mean(J1)
  m[,2] <- sd(J1)
  
  return(m)
  
 
}

#################################
# FUNCTION: curve_spc
# packages: vegan
# purpose: function to use spec accumulation curve from vegan, so i can use it via apply
# input: df
# output: 
# -------------------------------
curve_spc <- function(x=NULL) {
  
  z <- specaccum(x, method = "random", permutations = 1000)
  
  return(z)
}

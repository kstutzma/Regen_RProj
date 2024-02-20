#################################
# FUNCTION: asnum_keepname
# packages: none
# purpose: to keep column names while convering data frame to numeric
# input:
# output:
# -------------------------------
asnum_keepname <- function(m){
  
  m2 <- apply(m, 2, function(x) as.numeric(paste(x)))
  rownames(m2) <- rownames(m)
  
  return(m2)
}
  
  






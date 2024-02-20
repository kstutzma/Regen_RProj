#################################
# FUNCTION: SSfilter
# packages: dplyr
# purpose: to fiter regional dataframe for unique site to provide unique info/identification to data summaries generated next
# input: dataframe
# output: sorted new dataframe
# -------------------------------
SSfilter <- function(x=NULL, y=NULL) {
  
  y <- x %>% 
    filter(Site == y)
  
 return(y)
  
}

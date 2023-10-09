#################################
# FUNCTION: site_region_treat3
# packages: dplyr
# purpose: adds identifying data to species summary
# input: two dataframes
# output: one dataframe with identifying info from the first
# -------------------------------
site_region_treat3 <- function(d=NULL, x=NULL) {
  
  x$Site <- d$Site[1:3]
  x$Region <- d$Region[1:3]
  x$Treat_Type <- d$Treat_Type[1:3]
  
  return(x)
 
}

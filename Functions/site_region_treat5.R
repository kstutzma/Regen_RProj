#################################
# FUNCTION: site_region_treat5
# packages: dplyr
# purpose: adds identifying data to species summary
# input: two dataframes
# output: one dataframe with identifying info from the first
# -------------------------------
site_region_treat5 <- function(d=NULL, x=NULL) {
  
  x$Site <- d$Site[1:5]
  x$Region <- d$Region[1:5]
  x$Treat_Type <- d$Treat_Type[1:5]
  
  return(x)
 
}

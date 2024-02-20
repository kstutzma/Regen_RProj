#################################
# FUNCTION: site_region_treat1
# packages: dplyr
# purpose: adds identifying data to species summary
# input: two dataframes
# output: one dataframe with identifying info from the first
# -------------------------------
site_region_treat <- function(d=NULL, x=NULL, y=NULL) {
  
  x$Site <- d$Site[1:y]
  x$Region <- d$Region[1:y]
  x$Treat_Type <- d$Treat_Type[1:y]
  
  return(x)
 
}

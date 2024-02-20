#################################
# FUNCTION: site_region_treat_ba
# packages: dplyr
# purpose: adds identifying data to species summary
# input: two dataframes
# output: one dataframe with identifying info from the first
# -------------------------------
site_region_treat_ba <- function(d=NULL, x=NULL, y=NULL) {
  
  x$Site <- d$Site[1:y]
  x$Region <- d$Region[1:y]
  x$Treat_Type <- d$Treat_Type[1:y]
  x$Total_BA_A <- sum((x$BA_sf_a)[1:y])
  x$Total_BA_H <- sum((x$BA_sm_ha)[1:y])
  x$Prop_Total <- round((x$BA_sf_a/x$Total_BA_A), digits=3)
  
  return(x)
 
}

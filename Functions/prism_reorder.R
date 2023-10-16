#################################
# FUNCTION: prism_reorder
# packages: dplyr
# purpose: reorder prism data
# input: dataframe
# output: reorded data fram
# -------------------------------
prism_reorder <- function(x=NULL) {
  
  x <- x %>% 
    select(Region,
           Treat_Type,
           Mean_Site_BA_met,
           everything())
  
  return(x)
  
}

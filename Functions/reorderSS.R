#################################
# FUNCTION: reorderSS
# packages: dplyr
# purpose: to reorder dataframe before exporting to an excel
# input: dataframe
# output: reordered dataframe
# -------------------------------
reorderSS <- function( x = NULL) {
  
  x <- x %>% 
    select(Region, 
           Treat_Type, 
           Species_Groups,
           Species_Total,
           Group_Mean,
           Per_HA,
           No_of_Sites,
           everything())
  
  return(x)
  
}
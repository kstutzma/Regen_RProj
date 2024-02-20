#################################
# FUNCTION: LS_overallsummary
# packages: dplyr
# purpose: to combine regional data to get over all mean, number of sites a species shows up at, to get a regional species mean and ha counts
# input: dataframe
# output: dataframe
# -------------------------------
LS_overallsummary <- function(x=NULL, y=NULL) {
  
  x <- x %>% 
    group_by(Species_Groups) %>% 
    summarise(Overall_Species_Total = sum(Species_Total),
              No_of_Regions = n_distinct(Region)) %>%
    dplyr::mutate(Overall_Species_Mean = Overall_Species_Total/y) %>% 
    dplyr::mutate(Overall_per_HA = Overall_Species_Mean*1000)
  
  return(x)
  
  
}

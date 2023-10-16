#################################
# FUNCTION: prism_overallsummary
# packages: dplry
# purpose: combine data from different regions to get overall summary for basal area within treatment types
# input: dataframe
# output: dataframe 
# -------------------------------
prism_overallsummary <- function(x=NULL, y=NULL) {
  
  x <- x %>% 
    group_by(Species_Groups) %>% 
    summarise(Spec_Total_E = sum(Avg_BA_eng),
              Spec_Total_M = sum(Avg_BA_met),
              Prop_Total = sum(Prop_of_Spec),
              No_of_Regions = n_distinct(Region)) %>% 
              dplyr::mutate(Overall_Spec_Avg_M = Spec_Total_M/y) %>% 
                dplyr::mutate(Overall_Proportion = Prop_Total/y)
                
  return(x)  

}

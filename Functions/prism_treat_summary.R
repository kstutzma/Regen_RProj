#################################
# FUNCTION: prism_treat_summary
# packages: dplyr 
# purpose: to take units of like treatment type in a region and average them to provide treat/region data for prism 
# input: dataframe
# output: new dataframe with avg BA/species, overall avg BA for treat/region, % of species by BA
# -------------------------------
prism_treat_summary <- function(d=NULL, x=NULL, y=NULL) {
  
 x <- d %>% 
    filter(Treat_Type == x) %>% 
    summarise(Avg_BA_eng = round((sum(BA_sf_a))/y, digits=2),
              Avg_BA_met =round((sum(BA_sm_ha))/y, digits=2)) %>% 
   dplyr::mutate(Mean_Site_BA_met = round(sum(Avg_BA_met), digits=0)) %>% 
    dplyr::mutate(Prop_of_Spec = (round((Avg_BA_met/Mean_Site_BA_met), digits=3))*100)
  
 return(x)
  
}

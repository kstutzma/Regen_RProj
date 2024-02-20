#################################
# FUNCTION: prism_unitsummary
# packages: dplyr 
# purpose: to provide square feet per acre and square meter per hectare information for each unit by species
# input:
# output:
# -------------------------------
prism_unitsummary <- function(d=NULL, x=NULL) {
  x <- d %>% 
    filter(Site == x) %>% 
    group_by(Species_Groups) %>%
    summarise(SumBA = sum(Live_Total),
              No_Plots = No_of_Plots) %>%
    dplyr::mutate(BA_sf_a = round(((SumBA*10)/No_Plots), digits=0)) %>% 
    dplyr::mutate(BA_sm_ha = round((BA_sf_a*0.2296), digits = 0)) %>% 
    distinct()
  
 return(x)
}

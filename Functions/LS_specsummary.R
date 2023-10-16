#################################
# FUNCTION: LS_specsummary
# packages: dyplr
# purpose: to summarize large seedling data and provide mean and proportions for total, browsed, and stump sprouts for each treatment type in each region
# input: data frame
# output: dataframe sorted by species and returning new total, proportion, and mean data
# -------------------------------
LS_specsummary <- function(d=NULL, x=NULL, y=NULL) {
  
  x <- d %>% 
    filter(Treat_Type == x) %>% 
    group_by(Species_Groups) %>% 
    summarise(Species_Total = sum(Mean_Total), 
              T_Prop_Browse = ((sum(Prop_Browse)/y)), 
              T_Prop_SS= ((sum(Prop_SS)/y)),
              No_of_Sites = n_distinct(Site)) %>% 
    dplyr::mutate(Group_Mean = Species_Total/y) %>% 
    dplyr::mutate(Per_HA = Group_Mean*1000)
  
  return(x)
  
}

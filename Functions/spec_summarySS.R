#################################
# FUNCTION: spec_summarySS
# packages: dplyr
# purpose: to summarize small seedling data and provide mean and proportions for total, browsed, germinate, and stump sprouts for each treatment type in each region
# input: dataframe
# output: dataframe sorted by species and returning new total, proportion, and mean data
# -------------------------------
spec_summarySS <- function(d=NULL, x=NULL, y=NULL) {
  if(is.null(d)){
    x <- runif(3)
    y_var<- sample(LETTERS, 3)
    d <- data.frame(x,y_var)
  }
  
  x <- d %>% 
    filter(Treat_Type == x) %>% 
    group_by(Species_Groups) %>% 
    summarise(Species_Total = sum(Mean_Total), 
            T_Prop_Browse = ((sum(Prop_Browse)/y)), 
            T_Prop_SS= ((sum(Prop_SS)/y)),
            T_Prop_Germ=((sum(Prop_Germ)/y)),
            No_of_Sites = n_distinct(Site)) %>% 
    dplyr::mutate(Group_Mean = Species_Total/y) %>% 
    dplyr::mutate(Per_HA = Group_Mean*10000)
  
  return(x)
}

#################################
# FUNCTION: plotsummarySS
# packages: dplyr
# purpose: to summarize small seedling data and provide mean and proportions for total, browsed, germinate, and stump sprouts
# input: dataframe
# output: dataframe sorted by species and returning new total, proportion, and mean data
# -------------------------------
plotsummarySS <- function(d=NULL, x=NULL) {
  if(is.null(d)){
    x <- runif(3)
    y_var<- sample(LETTERS, 3)
    d <- data.frame(x,y_var)
  }
  
  x <- d %>% 
    filter(Site == x) %>% 
    group_by(Species_Groups) %>% 
    summarize(SumTotal = sum(Total), 
              Mean_Total=SumTotal/No_of_Plots, 
              TotalBrowse = sum(Browsed), 
              Prop_Browse=TotalBrowse/SumTotal,  
              TotalStumpSprout = sum(StumpSprout), 
              Prop_SS=TotalStumpSprout/SumTotal, 
              TotalGerm = sum(Germinate), 
              Prop_Germ=TotalGerm/SumTotal) %>% 
    distinct()
  
  return(x)
}

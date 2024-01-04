# Stutzman
# Pitch pine regen project :: troubleshooting code
#  2023-10-18
  

# this is how to find why you have too many data rows , unique and distinct were giving different number of rows, which is how I found out I had transposed some numbers and was able to fix that in the original data -------------------

small_seedling_meta <- small_seedling %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)


small_seedling_meta2 <- small_seedling %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  unique()

z <- setdiff(small_seedling_meta, small_seedling_meta2)


z <- data.frame(table(small_seedling_meta2$Plot_No))




# this is code for looking at skew and kurtosis -------------------


ggplot(ls_merge2, aes(x=sqrt_Total))+
  geom_histogram(binwidth = .3)+
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))

ggplot(ls_merge2, aes(x=log_Total))+
  geom_histogram(binwidth = .3)+
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))

# boxplots
ggplot(ls_merge2, aes(x = Species_Groups, y=sqrt_Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))

ggplot(ls_merge2, aes(x = Species_Groups, y=log_Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))

# look at skew and kurtosis for PIRI in each actegory -------------------

ls_control <- ls_merge2 %>% 
  filter(Species_Groups == "PIRI",
         Treat_Type == "Control")

skewness(ls_control$Total)
skewness(ls_control$sqrt_Total)
skewness(ls_control$log_Total)
kurtosis(ls_control$Total)
kurtosis(ls_control$sqrt_Total)
kurtosis(ls_control$log_Total)
#log is best at skew = 6.3, kurtosis = 43.4

ls_fallrx <- ls_merge2 %>% 
  filter(Species_Groups == "PIRI",
         Treat_Type == "FallRx")

skewness(ls_fallrx$Total)
skewness(ls_fallrx$sqrt_Total)
skewness(ls_fallrx$log_Total)
kurtosis(ls_fallrx$Total)
kurtosis(ls_fallrx$sqrt_Total)
kurtosis(ls_fallrx$log_Total)
#log is best at s = 5.6, k = 35.2

ls_harvest <- ls_merge2 %>% 
  filter(Species_Groups == "PIRI",
         Treat_Type == "Harvest")

skewness(ls_harvest$Total)
skewness(ls_harvest$sqrt_Total)
skewness(ls_harvest$log_Total)
kurtosis(ls_harvest$Total)
kurtosis(ls_harvest$sqrt_Total)
kurtosis(ls_harvest$log_Total)
# log is best at s = 1.8, k = 5.4

ls_mowrx <- ls_merge2 %>% 
  filter(Species_Groups == "PIRI",
         Treat_Type == "MowRx")

skewness(ls_mowrx$Total)
skewness(ls_mowrx$sqrt_Total)
skewness(ls_mowrx$log_Total)
kurtosis(ls_mowrx$Total)
kurtosis(ls_mowrx$sqrt_Total)
kurtosis(ls_mowrx$log_Total)
# log is best at s= 8.3, k = 82.6

ls_springrx <- ls_merge2 %>% 
  filter(Species_Groups == "PIRI",
         Treat_Type == "SpringRx")

skewness(ls_springrx$Total)
skewness(ls_springrx$sqrt_Total)
skewness(ls_springrx$log_Total)
kurtosis(ls_springrx$Total)
kurtosis(ls_springrx$sqrt_Total)
kurtosis(ls_springrx$log_Total)
# no change between the 3 - only one piri recorded in 9 sites, s = 7.5, k = 57


rm(ls_control,
   ls_fallrx,
   ls_harvest,
   ls_mowrx,
   ls_springrx)






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
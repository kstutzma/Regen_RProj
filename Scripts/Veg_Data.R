# Stutzman
# Pitch pine regen project: ground cover data
#  2023-11-07

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

# functions -------------------


# global variables -------------------
file_folder_veg <- "CleanData/veg_CSV/"

# Body -------------------
file_names_veg <- list.files(path=file_folder_veg)

#for loop to read in the files, copied from Bio381 class notes/homework 12
for (i in seq_along(file_names_veg)) {
  assign(paste0(file_names_veg[i], i), 
         read.csv(paste0(file_folder_veg, file_names_veg[i])))
}

# adding treatment types -------------------
Veg_Control_Data.csv1$Treat_Type <- "Control"
Veg_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
Veg_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
Veg_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
Veg_Thinned_Data.csv5$Treat_Type <- "Harvest"

# merge into one data set and then remove -------------------

veg <- rbind(Veg_Control_Data.csv1,
             Veg_Fall_Burn_Data.csv2,
             Veg_Mow_Burn_Data.csv3,
             Veg_Spring_Burn_Data.csv4,
             Veg_Thinned_Data.csv5)

rm(Veg_Control_Data.csv1,
   Veg_Fall_Burn_Data.csv2,
   Veg_Mow_Burn_Data.csv3,
   Veg_Spring_Burn_Data.csv4,
   Veg_Thinned_Data.csv5)

# thinking about the 0 - 8 scale as a factor/ordinal variable and not actually numbers -------------------

veg2 <- veg

veg2$SP_Total_Cover <- factor(veg2$SP_Total_Cover, order = TRUE,
                              levels = c("1", "2", "3", "4", "5", "6", "7", "8"))
 
is.factor(veg2$SP_Total_Cover)



# adding a column to give an actual number for each species, which is the midpoint number for the cover class -------------------
veg2$SP_Cover_midpoint <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, veg2$SP_Total_Cover)[match(veg2$SP_Total_Cover, c(0,1,2,3,4,5,6,7,8))]



ggplot(veg2, aes(x=Treat_Type, fill=SP_Total_Cover))+
  geom_bar(position = "stack")+
  guides(x = guide_axis(angle = 65))
# lol, I have no idea how to look at this data, maybe pull out different species? whatever, it's in


#Thinking of creating a new data set where there is a total cover variable. Unsure if I should let it go above 100 or if I should change all values above 100 to 100. Maybe take a look and then decide



veg3 <- veg2 %>% 
  group_by(Plot_No) %>% 
  summarise(Veg_Total = sum(SP_Cover_midpoint))

#this has 65 entries that are over 100%; no sites have 0

#maybe log transform?

veg3$l.Veg_Total <- log(veg3$Veg_Total)

rm(veg)























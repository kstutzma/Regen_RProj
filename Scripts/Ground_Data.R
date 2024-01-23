# Stutzman
# Pitch pine regen project: ground data
#  2023-11-06

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

# functions -------------------

# global variables -------------------
file_folder_gr <- "CleanData/ground_CSV/"

# Body -------------------
file_names_gr <- list.files(path=file_folder_gr)

#for loop to read in the files, copied from Bio381 class notes/homework 12
for (i in seq_along(file_names_gr)) {
  assign(paste0(file_names_gr[i], i), 
         read.csv(paste0(file_folder_gr, file_names_gr[i])))
}

# adding treatment types -------------------
G_Control_Data.csv1$Treat_Type <- "Control"
G_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
G_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
G_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
G_Thinned_Data.csv5$Treat_Type <- "Harvest"

# merge into 1 dataset -------------------
ground <- rbind(G_Control_Data.csv1,
                G_Fall_Burn_Data.csv2,
                G_Mow_Burn_Data.csv3,
                G_Spring_Burn_Data.csv4,
                G_Thinned_Data.csv5)

# removed merged datasets -------------------
rm(G_Control_Data.csv1,
   G_Fall_Burn_Data.csv2,
   G_Mow_Burn_Data.csv3,
   G_Spring_Burn_Data.csv4,
   G_Thinned_Data.csv5)

# it's the >15 lines that are the problem - what do I want to do about them? ------------------- how many are there? - there are 4. I changed them to 15.5 cm

# now need to convert 0, 1, 2 ... data to midpoint of cover class -------------------

# 1	  <1% 	  of 1 square meter frame		0.5
# 2	  1-5%	  of 1 square meter frame		3
# 3	  6-10%	  of 1 square meter frame		8
# 4	  11-20%	of 1 square meter frame		15.5
# 5	  21-40%	of 1 square meter frame		30.5
# 6	  41-60%	of 1 square meter frame		50.5
# 7	  61-80%	of 1 square meter frame		70.5
# 8	  81-100%	of 1 square meter frame		90.5

ground2 <- ground

ground2$Lichen <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Lichen)[match(ground2$Lichen, c(0,1,2,3,4,5,6,7,8))]

ground2$Moss<- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Moss)[match(ground2$Moss, c(0,1,2,3,4,5,6,7,8))]

ground2$Road_Trail <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Road_Trail)[match(ground2$Road_Trail, c(0,1,2,3,4,5,6,7,8))]

ground2$Rock <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Rock)[match(ground2$Rock, c(0,1,2,3,4,5,6,7,8))]

ground2$Mineral_Soil <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Mineral_Soil)[match(ground2$Mineral_Soil, c(0,1,2,3,4,5,6,7,8))]

ground2$Wood <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Wood)[match(ground2$Wood, c(0,1,2,3,4,5,6,7,8))]

ground2$Litter_Duff <- c(0, 0.5, 3, 8, 15.5, 30.5, 50.5, 70.5, 90.5, ground2$Litter_Duff)[match(ground2$Litter_Duff, c(0,1,2,3,4,5,6,7,8))]

ground2$l.LD1 <- log(ground2$LitterDepth1_cm + 1)
ground2$l.LD2 <- log(ground2$LitterDepth2_cm + 1)
ground2$avgLD <-((ground2$LitterDepth1_cm + ground2$LitterDepth2_cm)/2)
ground2$avgLD_l <-log(1+ground2$avgLD)
ground2$l.Mineral <- log(ground2$Mineral_Soil + 1)

ground3 <- ground2 %>% 
  select(Plot_No, Lichen, Moss, Road_Trail, Rock, Mineral_Soil, Wood, Litter_Duff, l.LD1, l.LD2, avgLD_l, l.Mineral, avgLD)

rm(ground2)

# ****************** Code I'm not using and don't want to run, but don't quite want to get rid of (which is why I like markdown files)

# code to pivot LL measurements long, don't think this come in handy, but keeping JIC

# litter_measure_long <- ground2 %>% 
#   pivot_longer(cols = LitterDepth1_cm:LitterDepth2_cm,
#                names_to = "Measurement",
#                values_to = "Litter_Depth_cm") %>% 
#   select(Region, Site, Plot_No, Measurement, Litter_Depth_cm, Treat_Type )
# 
# litter_long2 <- litter_measure_long %>% 
#   select(Plot_No, Measurement, Litter_Depth_cm)

#another pivot longer that doesn't work
# ground_long <- ground2 %>% 
#   pivot_longer(cols = LitterDepth1_cm:LitterDepth2_cm,
#                names_to = "Measurement",
#                values_to = "Litter_Depth_cm")




# early graphical representations of the data -------------------
# ggplot(litter_measure_long, aes(x=Litter_Depth_cm))+
#   geom_bar(linewidth = 2)+
#   facet_wrap(~Treat_Type)+
#   guides(x = guide_axis(angle = 65))
# 
# ggplot(litter_measure_long, aes(x=Treat_Type, y=Litter_Depth_cm))+
#   geom_boxplot()
# 
# ggplot(litter_measure_long, aes(x=Treat_Type, fill=Mineral_Soil))+
#   geom_bar(position = "fill") +
#   guides(x = guide_axis(angle = 65))

#rm(ground2, litter_measure_long)


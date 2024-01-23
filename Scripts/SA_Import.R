# Stutzman
# Sapling data import and manipulate/ready for analysis script 
#  2024-01-11

# I wanted to create one script to import sapling data and ready it for analysis, which could then be sourced at the beginning of all analysis scripts:


# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)



# functions -------------------




# global variables -------------------

file_folder_SA <- "CleanData/sapling_CSV/"




# Body -------------------

options(scipen = 999) #to display actual values

file_names_SA <- list.files(path=file_folder_SA)

for (i in seq_along(file_names_SA)) {
  assign(paste0(file_names_SA[i], i),
         read.csv(paste0(file_folder_SA, file_names_SA[i])))
}

# Adding treatment types -------------------

SA_Control_Data.csv1$Treat_Type <- "Control"
SA_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
SA_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
SA_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
SA_Thinned_Data.csv5$Treat_Type <- "Harvest"

# merge into one data set -------------------

sapling <- rbind(SA_Control_Data.csv1, 
                 SA_Fall_Burn_Data.csv2, 
                 SA_Mow_Burn_Data.csv3, 
                 SA_Spring_Burn_Data.csv4, 
                 SA_Thinned_Data.csv5) 


# remove data sets to keep environment clean-------------------

rm(SA_Control_Data.csv1,
   SA_Fall_Burn_Data.csv2,
   SA_Mow_Burn_Data.csv3,
   SA_Spring_Burn_Data.csv4,
   SA_Thinned_Data.csv5)


# change species groupings, going to stick with two here, if transformation works, I can always go back and make more groups     -------------------


# PIRI
# Other

sapling$Species_Groups <- NA

# PIRI
sapling$Species_Groups <- ifelse(sapling$Species_Code == 'PIRI', 'PIRI', sapling$Species_Groups)

# Shrub Oak
sapling$Species_Groups <- ifelse(sapling$Species_Code %in% c('QUIL','QUPR'), 'Shrub_Oak', sapling$Species_Groups)

#Other  
sapling$Species_Groups <- ifelse(sapling$Species_Code %in% c('ACRU', 'BEPO', 'QUCO', 'QUAL', 'QUVE', 'QUST', 'QURU', 'QUPR', 'PRSE', 'FRPE', 'PRVI', 'PODE', 'POGR', 'SAAL', 'AMSP', 'FAGR', 'NYSY', 'ACPE', 'ACNE', 'MOSP', 'POTR', 'PRSP', 'PIST', 'ABBA', 'FRAL', 'ROPS', 'RHCA', 'ELUM', 'BELE'), 'Other', sapling$Species_Groups)


# now to check all have been assigned -------------------
check_sa <- sapling %>% 
  filter(if_any(Species_Groups, is.na))

# there are 724 plots that do not have saplings recorded -------------------

rm(check_sa)

sapling2 <- sapling %>%
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups, 
         DBH.cm.)

# now what to do with it? Look into TPA data (T/HA) -------------------

# looks like I should have just tallied them and not recorded their DBH, maybe I'll condense into DBH categories? there really aren't a lot of saplings recorded, so that might break things up too much? -------------------

sapling2$Tally <- NA

sapling2$Tally <- ifelse(sapling2$DBH.cm. >= 2.5, 1, sapling2$Tally)

sapling2$Tally <- ifelse(sapling2$DBH.cm. == 0, 0, sapling2$Tally)

# check to see all have been assigned -------------------
check_sapling <- sapling2 %>% 
  filter(if_any(Tally, is.na))

rm(check_sapling)


# add duplicates by plot and filter it down -------------------

sapling3 <- sapling2 %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Tally)


n_distinct(sapling3$Plot_No)
# 1001 plots, yay

sapling_meta <- sapling3 %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

sapling_plot <- sapling3 %>% 
  select(Plot_No, Species_Groups, Tally)

#duplicates of "other need to be combined
sapling_plot1 <- sapling_plot %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Total_Tally = sum(Tally)) %>% 
  ungroup()


sapling_plot2 <- sapling_plot1 %>% 
  complete(Plot_No, Species_Groups, fill = list(Total_Tally = 0))

sapling_NA <- sapling_plot2[complete.cases(sapling_plot2),]

sapling_merge <- merge(sapling_meta, sapling_NA, by= "Plot_No")

# now remove datasets no longer needed to keep envr clean -------------------
rm(sapling_meta,
   sapling_plot,
   sapling_plot1,
   sapling_plot2,
   sapling2,
   sapling3,
   sapling_NA)

#Now the sapling data has been imported, labeled, and sorted. Species have been grouped and totaled and implied 0s have become express for all 1001 plots.
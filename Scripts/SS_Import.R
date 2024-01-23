# Stutzman
# Small Seedling data importation and manipulation script
#  2024-01-11

# I wanted to create a script that will import the small seedling data and prepare it for analysis, which then can be sourced at the beginning of all analysis scripts:

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)



# functions -------------------




# global variables -------------------
file_folder_SS <- "CleanData/smallseedling_CSV/"




# Body -------------------
options(scipen = 999)

file_names_SS <- list.files(path=file_folder_SS)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_SS)) {
  assign(paste0(file_names_SS[i], i),
         read.csv(paste0(file_folder_SS, file_names_SS[i])))
}

# Adding treatment types ------------------- not loading in SPB data, due to few regional replicates

SS_Control_Data.csv1$Treat_Type <- "Control"
SS_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
SS_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
SS_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
SS_Thinned_Data.csv5$Treat_Type <- "Harvest"


# merge into one data set -------------------

small_seedling <- rbind(SS_Control_Data.csv1, 
                        SS_Fall_Burn_Data.csv2, 
                        SS_Mow_Burn_Data.csv3, 
                        SS_Spring_Burn_Data.csv4,
                        SS_Thinned_Data.csv5) 

# remove merged data sets to keep envr clean -------------------
rm(SS_Control_Data.csv1, 
   SS_Fall_Burn_Data.csv2, 
   SS_Mow_Burn_Data.csv3, 
   SS_Spring_Burn_Data.csv4,
   SS_Thinned_Data.csv5)


# Changing species grouping -------------------
# PIRI
# Other: ACRU, QUIL, QUPR, QUCO, QUAL, QUVE, QURU, AMPS, BELE, BEPO, BESP, PODE, POGR, POTR, MASP, NYSY, ACPE, CASP, CRSP, FAGR, FRAM, FRAL, ROPS, RHCA, PRSE, PRVI, PRPE, PRSP, PIST, JUCO

# create a new empty column and then re-assign groups ------------------- 

small_seedling$Species_Groups <- NA

# PIRI	
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code == 'PIRI', 'PIRI', small_seedling$Species_Groups)

# Shrub Oak
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code %in% c('QUPR', 'QUIL'), 'Shrub_Oak', small_seedling$Species_Groups)

# Other
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code %in% c('ACRU', 'QUVE', 'QUAL', 'QURU', 'QUCO', 'AMSP', 'BELE', 'BEPO', 'BESP', 'PODE', 'POGR', 'POTR', 'MASP', 'NYSY', 'ACPE', 'CASP', 'CRSP', 'FAGR', 'FRAM', 'FRAL', 'ROPS', 'RHCA', 'SAAL', 'SAHU', 'PRSE', 'PRVI', 'PRPE','PRSP', 'PIST', 'JUCO'), 'Other', small_seedling$Species_Groups)

# now to check -------------------
check_SS <- small_seedling %>% 
  filter(if_any(Species_Groups, is.na))

# select for just the information I want, can lose species code and latin name -------------------
small_seedling1 <- small_seedling %>% 
  arrange(Treat_Type) %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Total,
         Browsed,
         StumpSprout,
         Germinate)

# I need to split the treat type, site, region data off or else it adds way too much info when i use 'complete' below -------------------

small_seedling_meta <- small_seedling1 %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)


small_seedling_plot <- small_seedling1 %>% 
  select(Plot_No, Species_Groups, Total, Browsed, StumpSprout, Germinate)


# duplicates of "other' etc need to be combined into one row before using complete -------------------
small_seedling_plot1 <- small_seedling_plot %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Total = sum(Total),
            Browsed = sum(Browsed),
            StumpSprout = sum(StumpSprout),
            Germinate = sum(Germinate)) %>% 
  ungroup()


small_seedling_plot2 <- small_seedling_plot1 %>% 
  complete(Plot_No, Species_Groups, fill = list(Total = 0, Browsed = 0, StumpSprout = 0, Germinate = 0))

# so that worked the way I wanted it to, I check a coupe of random plots against the original data. It does now carry an NA row for each point, which I will get rid of now -------------------

small_seedlingNA <- small_seedling_plot2[complete.cases(small_seedling_plot2),] 

# now to join it back with the meta data -------------------

ss_merge <- merge(small_seedling_meta, small_seedlingNA, by = "Plot_No")

n_distinct(ss_merge$Plot_No)
# this yields 1001  plots

# remove dataframes no longer needed -------------------
rm(small_seedling,
   small_seedling_meta,
   small_seedling_plot,
   small_seedling_plot1,
   small_seedling_plot2,
   small_seedlingNA,
   check_SS)

#Now the small seedling data has been imported, labeled, and sorted. Species have been grouped and totaled and implied 0s have become express for all 1001 plots.
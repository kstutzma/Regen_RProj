# Stutzman
# Pitch Pine Regeneration Project : Large Seedling code
#  2023-10-04

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)
library(doBy)

# source functions -------------------
# source("Functions/LS_plotsummary.R")
# source("Functions/SSfilter.R")
# source("Functions/site_region_treat.R")
# source("Functions/LS_specsummary.R")
# source("Functions/reorderSS.R")
# source("Functions/LS_overallsummary.R")

# global variables -------------------

file_folder_LS <- "CleanData/largeseedling_CSV/"

# Body -------------------

file_names_LS <- list.files(path=file_folder_LS)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_LS)) {
  assign(paste0(file_names_LS[i], i),
         read.csv(paste0(file_folder_LS, file_names_LS[i])))
}

# Adding treatment types -------------------

LS_Control_Data.csv1$Treat_Type <- "Control"
LS_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
LS_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
LS_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
LS_Thinned_Data.csv5$Treat_Type <- "Harvest"

# merge into one data set -------------------

large_seedling <- rbind(LS_Control_Data.csv1, 
                        LS_Fall_Burn_Data.csv2, 
                        LS_Mow_Burn_Data.csv3,
                        LS_Spring_Burn_Data.csv4, 
                        LS_Thinned_Data.csv5)

# remove merged datasets to keep the envr clean -------------------

rm(LS_Control_Data.csv1, 
   LS_Fall_Burn_Data.csv2, 
   LS_Mow_Burn_Data.csv3, 
   LS_Spring_Burn_Data.csv4, 
   LS_Thinned_Data.csv5)



# change species groupings -------------------

#these are the new groups:

# ACRU	
# PIRI	
# Shrub Oaks ::	QUIL, QUPR
# Tree Oak ::	QUCO, QUAL, QUVE, QURU, QUPA
# OTHER HARDWOOD ::	SAAL, AMPS, BELE, BEPO, BENI, PODE, POGR, POTR, ACPE, FRAM, SAHU, FAGR, MASP, ACNE, CRSP, FRAL, ROPS, RHCA, ELUM, PRSE, PRVI, PRPE
# OTHER SOFTWOOD ::	PIST, ABBA

large_seedling$Species_Groups <- NA

# ACRU
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code == 'ACRU', 'ACRU', large_seedling$Species_Groups)

# PIRI
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code == 'PIRI', 'PIRI', large_seedling$Species_Groups)

# Shrub Oaks ::	QUIL, QUPR
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('QUIL', 'QUPR'), 'Shrub_Oak', large_seedling$Species_Groups)

# Tree Oak ::	QUCO, QUAL, QUVE, QURU, QUPA
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('QUCO', 'QUAL', 'QUVE', 'QURU', 'QUPA'), 'Tree_Oak', large_seedling$Species_Groups)

# OTHER HARDWOOD ::	SAAL, AMPS, BELE, BEPO, BENI, PODE, POGR, POTR, ACPE, FRAM, SAHU, FAGR, MASP, ACNE, CRSP, FRAL, ROPS, RHCA, ELUM, PRSE, PRVI, PRPE
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('SAAL', 'AMSP', 'BELE', 'BEPO', 'BENI', 'PODE', 'POGR', 'POTR', 'ACPE', 'FRAM', 'SAHU', 'FAGR', 'MASP', 'ACNE', 'CRSP', 'FRAL', 'ROPS', 'RHCA', 'ELUM', 'FRPE', 'PRSE', 'PRVI', 'PRPE'), 'Other_Hardwood', large_seedling$Species_Groups)

# OTHER SOFTWOOD ::	PIST, ABBA
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('PIST', 'ABBA'), 'Other_Softwood', large_seedling$Species_Groups)


# now to check that all species have been assigned and only NAs are NAs in new column -------------------

check_ls <- large_seedling %>% 
  filter(if_any(Species_Groups, is.na))


# select for just the information I want, can lose species code and latin name -------------------

large_seedling <- large_seedling %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Total,
         Browsed,
         StumpSprout)

# maybe use plyr package to get summary stats and skew/kurtosis data? 


large_seedling_meta <- large_seedling %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

large_seedling_plot <- large_seedling %>% 
  select(Plot_No, Species_Groups, Total, Browsed, StumpSprout)

# duplicates of "other_hardwood' etc need to be combined into one row before using complete -------------------
large_seedling_plot1 <- large_seedling_plot %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Total = sum(Total),
            Browsed = sum(Browsed),
            StumpSprout = sum(StumpSprout)) %>% 
  ungroup()

large_seedling_plot2 <- large_seedling_plot1 %>% 
  complete(Plot_No, Species_Groups, fill = list(Total = 0, Browsed = 0, StumpSprout = 0))

# get rid of NAs -------------------
large_seedlingNA <- large_seedling_plot2[complete.cases(large_seedling_plot2),]

# now to join it back with the meta data -------------------
ls_merge <- merge(large_seedling_meta, large_seedlingNA, by = 'Plot_No')

# remove dataframes no longer needed -------------------
rm(check_ls,
   large_seedling,
   large_seedling_meta,
   large_seedling_plot,
   large_seedling_plot1,
   large_seedling_plot2,
   large_seedlingNA)




# exploring total data using histogram -------------------
ggplot(ls_merge, aes(x=Total))+
  geom_histogram()+
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))



# find mean for each site and create a histogram of that information -------------------

ls_merge2 <- ls_merge %>% 
  group_by(Treat_Type, Site, Species_Groups) %>% 
  summarise(Site_avg = mean(Total),
            Site_sd = sd(Total))



ggplot(ls_merge2, aes(x=Site_avg))+
  geom_histogram(binwidth = 1)+
  xlim(0, 20) +
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))






x <- summaryBy(Total + Browsed + StumpSprout ~ Treat_Type + Species_Groups, 
          data = ls_merge,
          FUN = summary)            

# this is a lot, so just looking at total count -------------------

x2 <- summaryBy(Total ~ Treat_Type + Species_Groups, 
               data = ls_merge,
               FUN = summary)


# how to do this but with dplyr -------------------
# group_by(dat, Species) %>%
#   summarise(
#     mean = mean(Sepal.Length, na.rm = TRUE),
#     sd = sd(Sepal.Length, na.rm = TRUE)
#   )


# now I'd like to examine skew and kurtosis then maybe log transform and look again? describeBy does not allow two grouping variables -------------------

MA_LS <- ls_merge %>% 
  filter(Region == 'MA')

describeBy(MA_LS, MA_LS$Species_Groups)
MA_LS$log_total <- log(MA_LS$Total)




LI_LS <- ls_merge %>% 
  filter(Region == 'LI')

ALB_LS <- ls_merge %>% 
  filter(Region == 'ALB')

NH_LS <- ls_merge %>% 
  filter(Region == 'NH')

ME_LS <- ls_merge %>% 
  filter(Region == 'ME')



















# remove NAs -------------------

large_seedlingNA <- large_seedling[complete.cases(large_seedling),]

# removed datasets to keep envr neat -------------------
rm(check_ls,
   large_seedling)


# Stutzman
# Pitch Pine Regen Project : Small seedling code
#  2023-10-04

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)

# functions -------------------
source("Functions/plotsummarySS.R")
source("Functions/site_region_treat1.R")
source("Functions/site_region_treat2.R")
source("Functions/site_region_treat3.R")
source("Functions/site_region_treat4.R")
source("Functions/site_region_treat5.R")
source("Functions/site_region_treat6.R")
source("Functions/site_region_treat7.R")
source("Functions/SSfilter.R")

# global variables -------------------

file_folder_SS <- "CleanData/smallseedling_CSV/"

# Body -------------------

file_names_SS <- list.files(path=file_folder_SS)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_SS)) {
  assign(paste0(file_names_SS[i], i),
         read.csv(paste0(file_folder_SS, file_names_SS[i])))
  }

# Adding treatment types -------------------

SS_Control_Data.csv1$Treat_Type <- "Control"
SS_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
SS_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
SS_No_Treatment_SPB.csv4$Treat_Type <- "SPB"
SS_Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
SS_Thinned_Data.csv6$Treat_Type <- "Harvest"

# merge into one data set -------------------

small_seedling <- rbind(SS_Control_Data.csv1, SS_Fall_Burn_Data.csv2, SS_Mow_Burn_Data.csv3, SS_No_Treatment_SPB.csv4, SS_Spring_Burn_Data.csv5, SS_Thinned_Data.csv6) 

# get mean abundance and frequency to look at species -------------------

x1 <- small_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(meanAbundance = mean(Total), number_of_occurrences=n())

# print to excel file -------------------

write_xlsx(x1, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")


# now thinking about what is the true mean -------------------

small_seedling2 <- small_seedling %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot_No)) %>% 
  dplyr::mutate(T_Mean = Total/No_of_Plots)

# how to get a true mean (aka total / no of plots) for each site? 

z1 <- small_seedling2 %>% 
  group_by(Latin_Name) %>% 
  summarise(meanAbun = mean(T_Mean), no_occur = n())

z2 <- small_seedling2 %>% 
  group_by(Latin_Name) %>% 
  group_by(Treat_Type) %>% 
  summarise(meanAbun = mean(T_Mean), no_occur = n())


# adding this data to the workbook -------------------

wb1 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")
addWorksheet(wb=wb1, sheetName = "SS_total/no_plots")
writeData(wb=wb1, sheet="SS_total/no_plots", x=z1, startCol = 1, startRow = 1)

addWorksheet(wb=wb1, sheetName = "SS_total/treatment")
writeData(wb=wb1, sheet="SS_total/treatment", x=z2, startCol = 1, startRow = 1)


saveWorkbook(wb=wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx", overwrite = TRUE)


# just want to see if I can get a total for the number of plots overall taken -------------------



nototalplots <- small_seedling %>% 
  dplyr::mutate(pl = n_distinct(Plot_No)) 

#I took 1,065 plots


# thinking what if i get a total and divide by overall number of plots? something about weighting is missing but I'm not quite sure what. it's a gross estimate but might be useful???





ss3 <- small_seedling %>% 
  group_by(Latin_Name) %>% 
  summarize(SUM = sum(Total), number_of_occurrences=n()) %>% 
  dplyr::mutate(T_Mean = SUM/1065) %>% 
  dplyr::mutate(Per_HA = T_Mean*10000)


# now to make this an excel -------------------

write_xlsx(ss3, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx")


invasive <- small_seedling %>% 
  filter(Species_Code == 'RHCA')



# Changing species grouping -------------------

# these are the new groups: 
# ACRU	
# PIRI	
# PR SP ::	PRSE, PRVI, PRPE, PRSP
# Shrub Oaks ::	QUIL, QUPR
# Tree Oak ::	QUCO, QUAL, QUVE, QURU
# HARDWOOD ::	AMPS, BELE, BEPO, BESP, PODE, POGR, POTR, MASP, NYSY, ACPE, CASP, CRSP, FAGR, FRAM, FRAL, ROPS, RHCA
# SOFTWOOD ::	PIST, JUCO

# create a new empty column and then re-assign groups ------------------- is there a faster way? the | symbol didn't work, maybe make a function?

small_seedling2$Species_Groups <- NA

# ACRU
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'ACRU', 'ACRU', small_seedling2$Species_Groups)

# PIRI	
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PIRI', 'PIRI', small_seedling2$Species_Groups)

# PR SP ::	PRSE, PRVI, PRPE, PRSP  
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PRSE', 'PRSP', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PRVI', 'PRSP', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PRPE', 'PRSP', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PRSP', 'PRSP', small_seedling2$Species_Groups)

# Shrub Oaks ::	QUIL, QUPR
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'QUIL', 'Shrub_Oak', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'QUPR', 'Shrub_Oak', small_seedling2$Species_Groups)

# Tree Oak ::	QUCO, QUAL, QUVE, QURU
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'QUCO', 'Tree_Oak', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'QUVE', 'Tree_Oak', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'QUAL', 'Tree_Oak', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'QURU', 'Tree_Oak', small_seedling2$Species_Groups)

# HARDWOOD ::	AMSP, BELE, BEPO, BESP, PODE, POGR, POTR, MASP, NYSY, ACPE, CASP, CRSP, FAGR, FRAM, FRAL, ROPS, RHCA, SAAL, SAHU
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'AMSP', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'BELE', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'BEPO', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'BESP', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PODE', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'POGR', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'POTR', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'MASP', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'NYSY', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'ACPE', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'CASp', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'CRSP', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'FAGR', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'FRAM', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'FRAL', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'ROPS', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'RHCA', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'SAAL', 'Other_Hardwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'SAHU', 'Other_Hardwood', small_seedling2$Species_Groups)

# SOFTWOOD ::	PIST, JUCO
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'PIST', 'Other_Softwood', small_seedling2$Species_Groups)
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code == 'JUCO', 'Other_Softwood', small_seedling2$Species_Groups)


# now to check -------------------
check_SS <- small_seedling2 %>% 
  filter(if_any(Species_Groups, is.na))

# remove NAs -------------------

small_seedlingNA <- small_seedling2[complete.cases(small_seedling2),]
  
# sort by region -------------------

MA_SS <- small_seedlingNA %>% 
  filter(Region == 'MA')

LI_SS <- small_seedlingNA %>% 
  filter(Region == 'LI')

ALB_SS <- small_seedlingNA %>% 
  filter(Region == 'ALB')

NH_SS <- small_seedlingNA %>% 
  filter(Region == 'NH')

ME_SS <- small_seedlingNA %>% 
  filter(Region == 'ME') %>% 

ME_SS <- select(ME_SS, -(T_Mean))
  
# remove unused data to make things cleaner -------------------

rm(SS_Control_Data.csv1, SS_Fall_Burn_Data.csv2, SS_Mow_Burn_Data.csv3, SS_No_Treatment_SPB.csv4, SS_Thinned_Data.csv6, SS_Spring_Burn_Data.csv5, small_seedling, small_seedling2, small_seedlingNA, ME_SS1)

# reorder dataframe -------------------
# ME_SS1 <- ME_SS %>% 
#   group_by(Site) %>% 
#   select(Region, Treat_Type, Site, Plot_No, Species_Groups, Total, Browsed, StumpSprout, Germinate, No_of_Plots, everything())


# # now to find means & proportions for each site -------------------
# TNCW_14 <- ME_SS1 %>% 
#   filter(Site == "TNCW_14") %>% 
#   group_by(Species_Groups) %>% 
#   summarize(SumTotal = sum(Total), 
#             Mean_Total=SumTotal/No_of_Plots, 
#             TotalBrowse = sum(Browsed), 
#             Prop_Browse=TotalBrowse/SumTotal,  
#             TotalStumpSprout = sum(StumpSprout), 
#             Prop_SS=TotalStumpSprout/SumTotal, 
#             TotalGerm = sum(Germinate), 
#             Prop_Germ=TotalGerm/SumTotal) %>% 
#   distinct()
#   
# # Add identifying information
#   TNCW_14$Site <- "TNCW_14"
#   TNCW_14$Region <- "ME"
#   TNCW_14$Treat_Type <- "Control"

# now the function SSfilter -------------------
# TNCW_14a <- ME_SS %>% 
#   filter(Site == "TNCW_14")
# TNCW_12a <- ME_SS %>% 
#   filter(Site == "TNCW_12")


#The above work has been condensed into these functions:

# MAINE DATA -------------------
TNCW_14a <- SSfilter(x=ME_SS, y="TNCW_14")
TNCW_14 <- plotsummarySS(d=ME_SS, x="TNCW_14")
TNCW_14 <- site_region_treat5(x=TNCW_14, d=TNCW_14a)

TNCW_12a <- SSfilter(x=ME_SS, y="TNCW_12")
TNCW_12 <- plotsummarySS(d=ME_SS, x="TNCW_12")
TNCW_12 <- site_region_treat6(x=TNCW_12, d=TNCW_12a)

TNCW_7SWa <- SSfilter(x=ME_SS, y="TNCW_7SW")
TNCW_7SW <- plotsummarySS(d=ME_SS, x="TNCW_7SW")
TNCW_7SW <- site_region_treat5(x=TNCW_7SW, d=TNCW_7SWa)

TNCW_18a <- SSfilter(x=ME_SS, y="TNCW_18")
TNCW_18 <- plotsummarySS(d=ME_SS, x="TNCW_18")
TNCW_18 <- site_region_treat5(x=TNCW_18, d=TNCW_18a)

TNCW_10SEa <- SSfilter(x=ME_SS, y="TNCW_10SE")
TNCW_10SE <- plotsummarySS(d=ME_SS, x="TNCW_10SE")
TNCW_10SE <- site_region_treat5(x=TNCW_10SE, d=TNCW_10SEa)

# combine Maine data into one dataframe and export to an excel -------------------

ME_SS_Sum_Data <- rbind(TNCW_10SE,
      TNCW_12,
      TNCW_14,
      TNCW_18,
      TNCW_7SW)

write_xlsx(ME_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/ME_SS_Summary_Data.xlsx")


# remove superfluous datasets to keep things neat -------------------
rm(TNCW_10SE, 
   TNCW_10SEa, 
   TNCW_12, 
   TNCW_12a, 
   TNCW_14, 
   TNCW_14a, 
   TNCW_18, 
   TNCW_18a, 
   TNCW_7SW, 
   TNCW_7SWa)


# calculate site means by treat category -------------------

ME_SS_FallRX <- ME_SS_Sum_Data %>% 
  filter(Treat_Type == "FallRx") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

ME_SS_FallRX$Region <- "ME"
ME_SS_FallRX$Treat_Type <- "FallRX"

write_xlsx(ME_SS_FallRX, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_FallRx.xlsx")


ME_SS_Control <- ME_SS_Sum_Data %>% 
  filter(Treat_Type == "Control") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/2) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

ME_SS_Control$Region <- "ME"
ME_SS_Control$Treat_Type <- "Control"

write_xlsx(ME_SS_Control, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_Control.xlsx")




# NEW HAMPSHIRE DATA -------------------
TNCO_Zito2a <- SSfilter(x=NH_SS, y="TNCO_Zito2")
TNCO_Zito2 <- plotsummarySS(d=NH_SS, x="TNCO_Zito2")
TNCO_Zito2 <- site_region_treat5(x=TNCO_Zito2, d=TNCO_Zito2a)

TNCO_WB10a <- SSfilter(x=NH_SS, y="TNCO_WB10")
TNCO_WB10 <- plotsummarySS(d=NH_SS, x="TNCO_WB10")
TNCO_WB10 <- site_region_treat6(x=TNCO_WB10, d=TNCO_WB10a)

TNCO_B1a <- SSfilter(x=NH_SS, y="TNCO_B1")
TNCO_B1 <- plotsummarySS(d=NH_SS, x="TNCO_B1")
TNCO_B1 <- site_region_treat5(x=TNCO_B1, d=TNCO_B1a)

TNCO_Hobbs5a <- SSfilter(x=NH_SS, y="TNCO_Hobbs5")
TNCO_Hobbs5 <- plotsummarySS(d=NH_SS, x="TNCO_Hobbs5")
TNCO_Hobbs5 <- site_region_treat5(x=TNCO_Hobbs5, d=TNCO_Hobbs5a)

TNCO_ESDB2a <- SSfilter(x=NH_SS, y="TNCO_ESDB2")
TNCO_ESDB2 <- plotsummarySS(d=NH_SS, x="TNCO_ESDB2")
TNCO_ESDB2 <- site_region_treat4(x=TNCO_ESDB2, d=TNCO_ESDB2a)

TNCO_WB1a <- SSfilter(x=NH_SS, y="TNCO_WB1")
TNCO_WB1 <- plotsummarySS(d=NH_SS, x="TNCO_WB1")
TNCO_WB1 <- site_region_treat4(x=TNCO_WB1, d=TNCO_WB1a)

TNCO_ESDB1a <- SSfilter(x=NH_SS, y="TNCO_ESDB1")
TNCO_ESDB1 <- plotsummarySS(d=NH_SS, x="TNCO_ESDB1")
TNCO_ESDB1 <- site_region_treat3(x=TNCO_ESDB1, d=TNCO_ESDB1a)

SPNHF_Harmona <- SSfilter(x=NH_SS, y="SPNHF_Harmon")
SPNHF_Harmon <- plotsummarySS(d=NH_SS, x="SPNHF_Harmon")
SPNHF_Harmon <- site_region_treat3(x=SPNHF_Harmon, d=SPNHF_Harmona)


# combine New Hampshire data into one dataframe and export to an excel -------------------

NH_SS_Sum_Data <- rbind(TNCO_B1, 
                        TNCO_ESDB1, 
                        TNCO_ESDB2, 
                        TNCO_Hobbs5, 
                        TNCO_WB1, 
                        TNCO_WB10, 
                        TNCO_Zito2,
                        SPNHF_Harmon)

write_xlsx(ME_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/NH_SS_Summary_Data.xlsx")

# remove superfluous datasets to keep things neat -------------------
rm(TNCO_B1, 
   TNCO_ESDB1, 
   TNCO_ESDB2, 
   TNCO_Hobbs5, 
   TNCO_WB1, 
   TNCO_WB10, 
   TNCO_Zito2,
   SPNHF_Harmon,
   TNCO_B1a, 
   TNCO_ESDB1a, 
   TNCO_ESDB2a, 
   TNCO_Hobbs5a, 
   TNCO_WB1a, 
   TNCO_WB10a, 
   TNCO_Zito2a,
   SPNHF_Harmona)


# calculate site means by treat category -------------------

NH_SS_FallRX <- NH_SS_Sum_Data %>% 
  filter(Treat_Type == "FallRx") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

NH_SS_FallRx$Region <- "NH"
NH_SS_FallRx$Treat_Type <- "FallRX"

SS_wb1 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_FallRx.xlsx")
addWorksheet(wb=SS_wb1, sheetName = "NH")
writeData(wb=SS_wb1, sheet="NH", x=NH_SS_FallRX, startCol = 1, startRow = 1)

saveWorkbook(wb=SS_wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_FallRx.xlsx", overwrite = TRUE)


NH_SS_Control <- NH_SS_Sum_Data %>% 
  filter(Treat_Type == "Control") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/2) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

NH_SS_Control$Region <- "NH"
NH_SS_Control$Treat_Type <- "Control"

SS_wb2 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_Control.xlsx")
addWorksheet(wb=SS_wb2, sheetName = "NH")
writeData(wb=SS_wb2, sheet="NH", x=NH_SS_Control, startCol = 1, startRow = 1)

saveWorkbook(wb=SS_wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_Control.xlsx", overwrite = TRUE)


NH_SS_MowRx <- NH_SS_Sum_Data %>% 
  filter(Treat_Type == "MowRx") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

NH_SS_MowRx$Region <- "NH"
NH_SS_MowRx$Treat_Type <- "MowRX"

write_xlsx(NH_SS_MowRx, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_MowRx.xlsx")


# LONG ISLAND DATA -------------------

RPD2_B1a <- SSfilter(x=LI_SS, y="RPD2_B1")
RPD2_B1 <- plotsummarySS(d=LI_SS, x="RPD2_B1")
RPD2_B1 <- site_region_treat4(x=RPD2_B1, d=RPD2_B1a)

RPD2_B2a <- SSfilter(x=LI_SS, y="RPD2_B2")
RPD2_B2 <- plotsummarySS(d=LI_SS, x="RPD2_B2")
RPD2_B2 <- site_region_treat5(x=RPD2_B2, d=RPD2_B2a)

RPD2_B3a <- SSfilter(x=LI_SS, y="RPD2_B3")
RPD2_B3 <- plotsummarySS(d=LI_SS, x="RPD2_B3")
RPD2_B3 <- site_region_treat4(x=RPD2_B3, d=RPD2_B3a)

RPD2_B4a <- SSfilter(x=LI_SS, y="RPD2_B4")
RPD2_B4 <- plotsummarySS(d=LI_SS, x="RPD2_B4")
RPD2_B4 <- site_region_treat3(x=RPD2_B4, d=RPD2_B4a)

RPD2_B5a <- SSfilter(x=LI_SS, y="RPD2_B5")
RPD2_B5 <- plotsummarySS(d=LI_SS, x="RPD2_B5")
RPD2_B5 <- site_region_treat3(x=RPD2_B5, d=RPD2_B5a)

RPD2_B6a <- SSfilter(x=LI_SS, y="RPD2_B6")
RPD2_B6 <- plotsummarySS(d=LI_SS, x="RPD2_B6")
RPD2_B6 <- site_region_treat3(x=RPD2_B6, d=RPD2_B6a)

RPD2_B8a <- SSfilter(x=LI_SS, y="RPD2_B8")
RPD2_B8 <- plotsummarySS(d=LI_SS, x="RPD2_B8")
RPD2_B8 <- site_region_treat2(x=RPD2_B8, d=RPD2_B8a)

RPD2_B7a <- SSfilter(x=LI_SS, y="RPD2_B7")
RPD2_B7 <- plotsummarySS(d=LI_SS, x="RPD2_B7")
RPD2_B7 <- site_region_treat3(x=RPD2_B7, d=RPD2_B7a)

RPD2_B9a <- SSfilter(x=LI_SS, y="RPD2_B9")
RPD2_B9 <- plotsummarySS(d=LI_SS, x="RPD2_B9")
RPD2_B9 <- site_region_treat3(x=RPD2_B9, d=RPD2_B9a)

HCP_GIa <- SSfilter(x=LI_SS, y="HCP_GI")
HCP_GI <- plotsummarySS(d=LI_SS, x="HCP_GI")
HCP_GI <- site_region_treat1(x=HCP_GI, d=HCP_GIa)

SHCP_1a <- SSfilter(x=LI_SS, y="SHCP_1")
SHCP_1 <- plotsummarySS(d=LI_SS, x="SHCP_1")
SHCP_1 <- site_region_treat3(x=SHCP_1, d=SHCP_1a)

WNWR_BTa <- SSfilter(x=LI_SS, y="WNWR_BT")
WNWR_BT <- plotsummarySS(d=LI_SS, x="WNWR_BT")
WNWR_BT <- site_region_treat5(x=WNWR_BT, d=WNWR_BTa)


# combine Long Island data into one dataframe and export to an excel -------------------

LI_SS_Sum_Data <- rbind(RPD2_B1,
                        RPD2_B2,
                        RPD2_B3,
                        RPD2_B4,
                        RPD2_B5,
                        RPD2_B6,
                        RPD2_B7,
                        RPD2_B9,
                        RPD2_B8,
                        SHCP_1,
                        HCP_GI,
                        WNWR_BT)

write_xlsx(LI_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LI_SS_Summary_Data.xlsx")

# remove superfluous datasets to keep things neat -------------------

rm(RPD2_B1,
   RPD2_B2,
   RPD2_B3,
   RPD2_B4,
   RPD2_B5,
   RPD2_B6,
   RPD2_B7,
   RPD2_B9,
   RPD2_B8,
   SHCP_1,
   HCP_GI,
   WNWR_BT,
   RPD2_B1a,
   RPD2_B2a,
   RPD2_B3a,
   RPD2_B4a,
   RPD2_B5a,
   RPD2_B6a,
   RPD2_B7a,
   RPD2_B9a,
   RPD2_B8a,
   SHCP_1a,
   HCP_GIa,
   WNWR_BTa)


# calculate site means by treat category -------------------

LI_SS_Control <- LI_SS_Sum_Data %>% 
  filter(Treat_Type == "Control") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

LI_SS_Control$Region <- "LI"
LI_SS_Control$Treat_Type <- "Control"

SS_wb2 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_Control.xlsx")
addWorksheet(wb=SS_wb1, sheetName = "LI")
writeData(wb=SS_wb1, sheet="LI", x=LI_SS_Control, startCol = 1, startRow = 1)

saveWorkbook(wb=SS_wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_Control.xlsx", overwrite = TRUE)


LI_SS_Harvest <- LI_SS_Sum_Data %>% 
  filter(Treat_Type == "Harvest") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

LI_SS_Harvest$Region <- "LI"
LI_SS_Harvest$Treat_Type <- "Harvest"

write_xlsx(LI_SS_Harvest, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_Harvest.xlsx")


LI_SS_SPB <- LI_SS_Sum_Data %>% 
  filter(Treat_Type == "SPB") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

LI_SS_SPB$Region <- "LI"
LI_SS_SPB$Treat_Type <- "SPB"

write_xlsx(LI_SS_SPB, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_SPB.xlsx")


LI_SS_SpringRx <- LI_SS_Sum_Data %>% 
  filter(Treat_Type == "SpringRx") %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)

LI_SS_SpringRx$Region <- "LI"
LI_SS_SpringRx$Treat_Type <- "SpringRx"

write_xlsx(LI_SS_SpringRx, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SS_SpringRx.xlsx")


# Now for Albany Data -------------------






  
  
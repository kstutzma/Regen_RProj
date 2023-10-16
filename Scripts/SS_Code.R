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
source("Functions/spec_summarySS.R")
source("Functions/site_region_treat.R")
source("Functions/SSfilter.R")
source("Functions/reorderSS.R")
source("Functions/overall_summarySS.R")

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

# remove merged data sets to keep envr clean -------------------
rm(SS_Control_Data.csv1, SS_Fall_Burn_Data.csv2, SS_Mow_Burn_Data.csv3, SS_No_Treatment_SPB.csv4, SS_Spring_Burn_Data.csv5, SS_Thinned_Data.csv6)


# now thinking about what is the true mean -------------------

small_seedling2 <- small_seedling %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot_No)) %>% 
  dplyr::mutate(T_Mean = Total/No_of_Plots)



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
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code %in% c('PRSE','PRVI','PRPE','PRSP'), 'PRSP', small_seedling2$Species_Groups)

# Shrub Oaks ::	QUIL, QUPR
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code %in% c('QUIL', 'QUPR'), 'Shrub_Oak', small_seedling2$Species_Groups)


# Tree Oak ::	QUCO, QUAL, QUVE, QURU
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code %in% c('QUCO', 'QUVE', 'QUAL', 'QURU'), 'Tree_Oak', small_seedling2$Species_Groups)


# HARDWOOD ::	AMSP, BELE, BEPO, BESP, PODE, POGR, POTR, MASP, NYSY, ACPE, CASP, CRSP, FAGR, FRAM, FRAL, ROPS, RHCA, SAAL, SAHU
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code %in% c('AMSP', 'BELE', 'BEPO', 'BESP', 'PODE', 'POGR', 'POTR', 'MASP', 'NYSY', 'ACPE', 'CASP', 'CRSP', 'FAGR', 'FRAM', 'FRAL', 'ROPS', 'RHCA', 'SAAL', 'SAHU'), 'Other_Hardwood', small_seedling2$Species_Groups)

# SOFTWOOD ::	PIST, JUCO
small_seedling2$Species_Groups <- ifelse(small_seedling2$Species_Code %in% c('PIST', 'JUCO'), 'Other_Softwood', small_seedling2$Species_Groups)


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
  filter(Region == 'ME') 
  
# remove unused data to make things cleaner -------------------

rm(small_seedling,
   small_seedling2,
   small_seedlingNA,
   check_SS)



# MAINE DATA -------------------
TNCW_14a <- SSfilter(x=ME_SS, y="TNCW_14")
TNCW_14 <- plotsummarySS(d=ME_SS, x="TNCW_14")
TNCW_14 <- site_region_treat(x=TNCW_14, d=TNCW_14a, y=5)

TNCW_12a <- SSfilter(x=ME_SS, y="TNCW_12")
TNCW_12 <- plotsummarySS(d=ME_SS, x="TNCW_12")
TNCW_12 <- site_region_treat(x=TNCW_12, d=TNCW_12a, y=6)

TNCW_7SWa <- SSfilter(x=ME_SS, y="TNCW_7SW")
TNCW_7SW <- plotsummarySS(d=ME_SS, x="TNCW_7SW")
TNCW_7SW <- site_region_treat(x=TNCW_7SW, d=TNCW_7SWa, y=5)

TNCW_18a <- SSfilter(x=ME_SS, y="TNCW_18")
TNCW_18 <- plotsummarySS(d=ME_SS, x="TNCW_18")
TNCW_18 <- site_region_treat(x=TNCW_18, d=TNCW_18a, y=5)

TNCW_10SEa <- SSfilter(x=ME_SS, y="TNCW_10SE")
TNCW_10SE <- plotsummarySS(d=ME_SS, x="TNCW_10SE")
TNCW_10SE <- site_region_treat(x=TNCW_10SE, d=TNCW_10SEa, y=5)

# combine Maine data into one dataframe and export to an excel -------------------

ME_SS_Sum_Data <- rbind(TNCW_10SE,
      TNCW_12,
      TNCW_14,
      TNCW_18,
      TNCW_7SW)

write_xlsx(ME_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Summary_Data_SS_ME.xlsx")


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

# there is now a function to get mean total by species and then proportion of browsed, stump sprouts, and germinates
# function: spec_summarySS


ME_SS_FallRx <- spec_summarySS(d=ME_SS_Sum_Data, x="FallRx", y=3)
ME_SS_FallRx$Region <- "ME"
ME_SS_FallRx$Treat_Type <- "FallRX"
ME_SS_FallRx <- reorderSS(x=ME_SS_FallRx)


ME_SS_Control <- spec_summarySS(d=ME_SS_Sum_Data, x= "Control", y=2)
ME_SS_Control$Region <- "ME"
ME_SS_Control$Treat_Type <- "Control"
ME_SS_Control <- reorderSS(ME_SS_Control)



# NEW HAMPSHIRE DATA -------------------
TNCO_Zito2a <- SSfilter(x=NH_SS, y="TNCO_Zito2")
TNCO_Zito2 <- plotsummarySS(d=NH_SS, x="TNCO_Zito2")
TNCO_Zito2 <- site_region_treat(x=TNCO_Zito2, d=TNCO_Zito2a, y=5)

TNCO_WB10a <- SSfilter(x=NH_SS, y="TNCO_WB10")
TNCO_WB10 <- plotsummarySS(d=NH_SS, x="TNCO_WB10")
TNCO_WB10 <- site_region_treat(x=TNCO_WB10, d=TNCO_WB10a, y=6)

TNCO_B1a <- SSfilter(x=NH_SS, y="TNCO_B1")
TNCO_B1 <- plotsummarySS(d=NH_SS, x="TNCO_B1")
TNCO_B1 <- site_region_treat(x=TNCO_B1, d=TNCO_B1a, y=5)

TNCO_Hobbs5a <- SSfilter(x=NH_SS, y="TNCO_Hobbs5")
TNCO_Hobbs5 <- plotsummarySS(d=NH_SS, x="TNCO_Hobbs5")
TNCO_Hobbs5 <- site_region_treat(x=TNCO_Hobbs5, d=TNCO_Hobbs5a, y=5)

TNCO_ESDB2a <- SSfilter(x=NH_SS, y="TNCO_ESDB2")
TNCO_ESDB2 <- plotsummarySS(d=NH_SS, x="TNCO_ESDB2")
TNCO_ESDB2 <- site_region_treat(x=TNCO_ESDB2, d=TNCO_ESDB2a, y=4)

TNCO_WB1a <- SSfilter(x=NH_SS, y="TNCO_WB1")
TNCO_WB1 <- plotsummarySS(d=NH_SS, x="TNCO_WB1")
TNCO_WB1 <- site_region_treat(x=TNCO_WB1, d=TNCO_WB1a, y=4)

TNCO_ESDB1a <- SSfilter(x=NH_SS, y="TNCO_ESDB1")
TNCO_ESDB1 <- plotsummarySS(d=NH_SS, x="TNCO_ESDB1")
TNCO_ESDB1 <- site_region_treat(x=TNCO_ESDB1, d=TNCO_ESDB1a, y=3)

SPNHF_Harmona <- SSfilter(x=NH_SS, y="SPNHF_Harmon")
SPNHF_Harmon <- plotsummarySS(d=NH_SS, x="SPNHF_Harmon")
SPNHF_Harmon <- site_region_treat(x=SPNHF_Harmon, d=SPNHF_Harmona, y=3)


# combine New Hampshire data into one dataframe and export to an excel -------------------

NH_SS_Sum_Data <- rbind(TNCO_B1, 
                        TNCO_ESDB1, 
                        TNCO_ESDB2, 
                        TNCO_Hobbs5, 
                        TNCO_WB1, 
                        TNCO_WB10, 
                        TNCO_Zito2,
                        SPNHF_Harmon)

write_xlsx(NH_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Summary_Data_SS_NH.xlsx")

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

NH_SS_FallRx <- spec_summarySS(d=NH_SS_Sum_Data, x="FallRx", y=3) 
NH_SS_FallRx$Region <- "NH"
NH_SS_FallRx$Treat_Type <- "FallRx"
NH_SS_FallRx <- reorderSS(NH_SS_FallRx)


NH_SS_Control <- spec_summarySS(d=NH_SS_Sum_Data, x="Control", y=2)
NH_SS_Control$Region <- "NH"
NH_SS_Control$Treat_Type <- "Control"
NH_SS_Control <- reorderSS(NH_SS_Control)


NH_SS_MowRx <-spec_summarySS(d=NH_SS_Sum_Data, x="MowRx", y=3)
NH_SS_MowRx$Region <- "NH"
NH_SS_MowRx$Treat_Type <- "MowRX"
NH_SS_MowRx <- reorderSS(NH_SS_MowRx)



# LONG ISLAND DATA -------------------

RPD2_B1a <- SSfilter(x=LI_SS, y="RPD2_B1")
RPD2_B1 <- plotsummarySS(d=LI_SS, x="RPD2_B1")
RPD2_B1 <- site_region_treat(x=RPD2_B1, d=RPD2_B1a, y=4)

RPD2_B2a <- SSfilter(x=LI_SS, y="RPD2_B2")
RPD2_B2 <- plotsummarySS(d=LI_SS, x="RPD2_B2")
RPD2_B2 <- site_region_treat(x=RPD2_B2, d=RPD2_B2a, y=5)

RPD2_B3a <- SSfilter(x=LI_SS, y="RPD2_B3")
RPD2_B3 <- plotsummarySS(d=LI_SS, x="RPD2_B3")
RPD2_B3 <- site_region_treat(x=RPD2_B3, d=RPD2_B3a, y=4)

RPD2_B4a <- SSfilter(x=LI_SS, y="RPD2_B4")
RPD2_B4 <- plotsummarySS(d=LI_SS, x="RPD2_B4")
RPD2_B4 <- site_region_treat(x=RPD2_B4, d=RPD2_B4a, y=3)

RPD2_B5a <- SSfilter(x=LI_SS, y="RPD2_B5")
RPD2_B5 <- plotsummarySS(d=LI_SS, x="RPD2_B5")
RPD2_B5 <- site_region_treat(x=RPD2_B5, d=RPD2_B5a, y=3)

RPD2_B6a <- SSfilter(x=LI_SS, y="RPD2_B6")
RPD2_B6 <- plotsummarySS(d=LI_SS, x="RPD2_B6")
RPD2_B6 <- site_region_treat(x=RPD2_B6, d=RPD2_B6a, y=3)

RPD2_B8a <- SSfilter(x=LI_SS, y="RPD2_B8")
RPD2_B8 <- plotsummarySS(d=LI_SS, x="RPD2_B8")
RPD2_B8 <- site_region_treat(x=RPD2_B8, d=RPD2_B8a, y=2)

RPD2_B7a <- SSfilter(x=LI_SS, y="RPD2_B7")
RPD2_B7 <- plotsummarySS(d=LI_SS, x="RPD2_B7")
RPD2_B7 <- site_region_treat(x=RPD2_B7, d=RPD2_B7a, y=3)

RPD2_B9a <- SSfilter(x=LI_SS, y="RPD2_B9")
RPD2_B9 <- plotsummarySS(d=LI_SS, x="RPD2_B9")
RPD2_B9 <- site_region_treat(x=RPD2_B9, d=RPD2_B9a, y=3)

HCP_GIa <- SSfilter(x=LI_SS, y="HCP_GI")
HCP_GI <- plotsummarySS(d=LI_SS, x="HCP_GI")
HCP_GI <- site_region_treat(x=HCP_GI, d=HCP_GIa, y=1)

SHCP_1a <- SSfilter(x=LI_SS, y="SHCP_1")
SHCP_1 <- plotsummarySS(d=LI_SS, x="SHCP_1")
SHCP_1 <- site_region_treat(x=SHCP_1, d=SHCP_1a, y=3)

WNWR_BTa <- SSfilter(x=LI_SS, y="WNWR_BT")
WNWR_BT <- plotsummarySS(d=LI_SS, x="WNWR_BT")
WNWR_BT <- site_region_treat(x=WNWR_BT, d=WNWR_BTa, y=5)


# combine Long Island data into one dataframe and export to an excel -------------------

LI_SS_Sum_Data <- rbind(RPD2_B1,
                        RPD2_B2,
                        RPD2_B3,
                        RPD2_B4,
                        RPD2_B5,
                        RPD2_B6,
                        RPD2_B7,
                        RPD2_B8,
                        RPD2_B9,
                        SHCP_1,
                        HCP_GI,
                        WNWR_BT)

write_xlsx(LI_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Summary_Data_SS_LI.xlsx")

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

LI_SS_Control <- spec_summarySS(d=LI_SS_Sum_Data, x="Control", y=3)
LI_SS_Control$Region <- "LI"
LI_SS_Control$Treat_Type <- "Control"
LI_SS_Control <- reorderSS(LI_SS_Control)


LI_SS_Harvest <- spec_summarySS(d=LI_SS_Sum_Data, x="Harvest", y=3)
LI_SS_Harvest$Region <- "LI"
LI_SS_Harvest$Treat_Type <- "Harvest"
LI_SS_Harvest <- reorderSS(LI_SS_Harvest)


LI_SS_SPB <- spec_summarySS(d=LI_SS_Sum_Data, x="SPB", y=3)
LI_SS_SPB$Region <- "LI"
LI_SS_SPB$Treat_Type <- "SPB"
LI_SS_SPB <- reorderSS(LI_SS_SPB)


LI_SS_SpringRx <- spec_summarySS(d=LI_SS_Sum_Data, x="SpringRx", y=3)
LI_SS_SpringRx$Region <- "LI"
LI_SS_SpringRx$Treat_Type <- "SpringRx"
LI_SS_SpringRx <- reorderSS(LI_SS_SpringRx)



# Now for Albany Data -------------------
APB_Alleycata <- SSfilter(x=ALB_SS, y="APB_Alleycat")
APB_Alleycat <- plotsummarySS(d=ALB_SS, x="APB_Alleycat")
APB_Alleycat <- site_region_treat(x=APB_Alleycat, d=APB_Alleycata, y=3)

APB_Bivya <- SSfilter(x=ALB_SS, y="APB_Bivy")
APB_Bivy <- plotsummarySS(d=ALB_SS, x="APB_Bivy")
APB_Bivy <- site_region_treat(x=APB_Bivy, d=APB_Bivya, y=3)

APB_Chubb_Ea <- SSfilter(x=ALB_SS, y="APB_Chubb_E")
APB_Chubb_E <- plotsummarySS(d=ALB_SS, x="APB_Chubb_E")
APB_Chubb_E <- site_region_treat(x=APB_Chubb_E, d=APB_Chubb_Ea, y=6)

APB_Chubb_Wa <- SSfilter(x=ALB_SS, y="APB_Chubb_W")
APB_Chubb_W <- plotsummarySS(d=ALB_SS, x="APB_Chubb_W")
APB_Chubb_W <- site_region_treat(x=APB_Chubb_W, d=APB_Chubb_Wa, y=7)

APB_Dandya <- SSfilter(x=ALB_SS, y="APB_Dandy")
APB_Dandy <- plotsummarySS(d=ALB_SS, x="APB_Dandy")
APB_Dandy<- site_region_treat(x=APB_Dandy, d=APB_Dandya, y=6)

APB_Fowlersa <- SSfilter(x=ALB_SS, y="APB_Fowlers")
APB_Fowlers <- plotsummarySS(d=ALB_SS, x="APB_Fowlers")
APB_Fowlers <- site_region_treat(x=APB_Fowlers, d=APB_Fowlersa, y=5)

APB_Hippoa <- SSfilter(x=ALB_SS, y="APB_Hippo")
APB_Hippo <- plotsummarySS(d=ALB_SS, x="APB_Hippo")
APB_Hippo <- site_region_treat(x=APB_Hippo, d=APB_Hippoa, y=4)

APB_Hoffmana <- SSfilter(x=ALB_SS, y="APB_Hoffman")
APB_Hoffman <- plotsummarySS(d=ALB_SS, x="APB_Hoffman")
APB_Hoffman <- site_region_treat(x=APB_Hoffman, d=APB_Hoffmana, y=5)

APB_HuckMounda <- SSfilter(x=ALB_SS, y="APB_HuckMound")
APB_HuckMound <- plotsummarySS(d=ALB_SS, x="APB_HuckMound")
APB_HuckMound <- site_region_treat(x=APB_HuckMound, d=APB_HuckMounda, y=4)

APB_Humdingera <- SSfilter(x=ALB_SS, y="APB_Humdinger")
APB_Humdinger <- plotsummarySS(d=ALB_SS, x="APB_Humdinger")
APB_Humdinger <- site_region_treat(x=APB_Humdinger,
d=APB_Humdingera, y=5)

APB_Hydroa <- SSfilter(x=ALB_SS, y="APB_Hydro")
APB_Hydro <- plotsummarySS(d=ALB_SS, x="APB_Hydro")
APB_Hydro <- site_region_treat(x=APB_Hydro, d=APB_Hydroa, y=3)


# combine all Albany data into one dataframe and export an excel -------------------

ALB_SS_Sum_Data <- rbind(APB_Hippo,
                         APB_Hydro,
                         APB_Chubb_E,
                         APB_Chubb_W,
                         APB_Dandy,
                         APB_Fowlers,
                         APB_Hoffman,
                         APB_HuckMound,
                         APB_Humdinger,
                         APB_Alleycat,
                         APB_Bivy)

write_xlsx(ALB_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Summary_Data_SS_ALB.xlsx")


# remove now combined datasets to keep environment cleaner -------------------

rm(APB_Hippo,
   APB_Hydro,
   APB_Chubb_E,
   APB_Chubb_W,
   APB_Dandy,
   APB_Fowlers,
   APB_Hoffman,
   APB_HuckMound,
   APB_Humdinger,
   APB_Alleycat,
   APB_Bivy,
   APB_Hippoa,
   APB_Hydroa,
   APB_Chubb_Ea,
   APB_Chubb_Wa,
   APB_Dandya,
   APB_Fowlersa,
   APB_Hoffmana,
   APB_HuckMounda,
   APB_Humdingera,
   APB_Alleycata,
   APB_Bivya)

# calculate site means by treat category; APB is unique in that there is a summer moxrx and spring mow rx category each that need to be combined -------------------

ALB_SS_Control <- spec_summarySS(d=ALB_SS_Sum_Data, x="Control", y=2)
ALB_SS_Control$Region <- "ALB"
ALB_SS_Control$Treat_Type <- "Control"
ALB_SS_Control <- reorderSS(ALB_SS_Control)


ALB_SS_SpringRx <- spec_summarySS(d=ALB_SS_Sum_Data, x="SpringRx", y=3)
ALB_SS_SpringRx$Region <- "ALB"
ALB_SS_SpringRx$Treat_Type <- "SpringRx"
ALB_SS_SpringRx <- reorderSS(ALB_SS_SpringRx)


ALB_SS_Sp_MowRx <- ALB_SS_Sum_Data %>% 
  filter(Site %in% c("APB_Humdinger", "APB_Bivy", "APB_Alleycat")) %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), 
            T_Prop_Browse = ((sum(Prop_Browse)/3)), 
            T_Prop_SS= ((sum(Prop_SS)/3)),
            T_Prop_Germ=((sum(Prop_Germ)/3)),
            No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)
ALB_SS_Sp_MowRx$Region <- "ALB"
ALB_SS_Sp_MowRx$Treat_Type <- "MowRx"
ALB_SS_Sp_MowRx <- reorderSS(ALB_SS_Sp_MowRx)


ALB_SS_Su_MowRx <- ALB_SS_Sum_Data %>% 
  filter(Site %in% c("APB_Chubb_W", "APB_Chubb_E", "APB_Dandy")) %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), 
            T_Prop_Browse = ((sum(Prop_Browse)/3)), 
            T_Prop_SS= ((sum(Prop_SS)/3)),
            T_Prop_Germ=((sum(Prop_Germ)/3)),
            No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*10000)
ALB_SS_Su_MowRx$Region <- "ALB"
ALB_SS_Su_MowRx$Treat_Type <- "MowRx"
ALB_SS_Su_MowRx <- reorderSS(x=ALB_SS_Su_MowRx)



# Now for MASS Data -------------------

CE_B7_Ba <- SSfilter(x=MA_SS, y="CE_B7_B")
CE_B7_B <- plotsummarySS(d=MA_SS, x="CE_B7_B")
CE_B7_B <- site_region_treat(x=CE_B7_B, d=CE_B7_Ba, y=5)

CE_B7_Ca <- SSfilter(x=MA_SS, y="CE_B7_C")
CE_B7_C <- plotsummarySS(d=MA_SS, x="CE_B7_C")
CE_B7_C <- site_region_treat(x=CE_B7_C, d=CE_B7_Ca, y=4)

CE_B7_Da <- SSfilter(x=MA_SS, y="CE_B7_D")
CE_B7_D <- plotsummarySS(d=MA_SS, x="CE_B7_D")
CE_B7_D <- site_region_treat(x=CE_B7_D, d=CE_B7_Da, y=5)

CE_BA3a <- SSfilter(x=MA_SS, y="CE_BA3")
CE_BA3 <- plotsummarySS(d=MA_SS, x="CE_BA3")
CE_BA3 <- site_region_treat(x=CE_BA3, d=CE_BA3a, y=5)

CE_BP20a <- SSfilter(x=MA_SS, y="CE_BP20")
CE_BP20 <- plotsummarySS(d=MA_SS, x="CE_BP20")
CE_BP20 <- site_region_treat(x=CE_BP20, d=CE_BP20a, y=5)

CE_SWB_E1a <- SSfilter(x=MA_SS, y="CE_SWB_E1")
CE_SWB_E1 <- plotsummarySS(d=MA_SS, x="CE_SWB_E1")
CE_SWB_E1 <- site_region_treat(x=CE_SWB_E1, d=CE_SWB_E1a, y=6)

CE_SWB_Na <- SSfilter(x=MA_SS, y="CE_SWB_N")
CE_SWB_N <- plotsummarySS(d=MA_SS, x="CE_SWB_N")
CE_SWB_N <- site_region_treat(x=CE_SWB_N, d=CE_SWB_Na, y=4)

CE_SWB_Sa <- SSfilter(x=MA_SS, y="CE_SWB_S")
CE_SWB_S <- plotsummarySS(d=MA_SS, x="CE_SWB_S")
CE_SWB_S <- site_region_treat(x=CE_SWB_S, d=CE_SWB_Sa, y=5)

CE_Wheelocka <- SSfilter(x=MA_SS, y="CE_Wheelock")
CE_Wheelock <- plotsummarySS(d=MA_SS, x="CE_Wheelock")
CE_Wheelock <- site_region_treat(x=CE_Wheelock, d=CE_Wheelocka, y=4)

Mashpee_B1a <- SSfilter(x=MA_SS, y="Mashpee_B1")
Mashpee_B1 <- plotsummarySS(d=MA_SS, x="Mashpee_B1")
Mashpee_B1 <- site_region_treat(x=Mashpee_B1, d=Mashpee_B1a, y=5)

Mashpee_Ca <- SSfilter(x=MA_SS, y="Mashpee_C")
Mashpee_C <- plotsummarySS(d=MA_SS, x="Mashpee_C")
Mashpee_C <- site_region_treat(x=Mashpee_C, d=Mashpee_Ca, y=3)

Mashpee_G3a <- SSfilter(x=MA_SS, y="Mashpee_G3")
Mashpee_G3 <- plotsummarySS(d=MA_SS, x="Mashpee_G3")
Mashpee_G3 <- site_region_treat(x=Mashpee_G3, d=Mashpee_G3a, y=2)

MS_07aa <- SSfilter(x=MA_SS, y="MS_07a")
MS_07a <- plotsummarySS(d=MA_SS, x="MS_07a")
MS_07a <- site_region_treat(x=MS_07a, d=MS_07aa, y=5)

MS_09da <- SSfilter(x=MA_SS, y="MS_09d")
MS_09d <- plotsummarySS(d=MA_SS, x="MS_09d")
MS_09d <- site_region_treat(x=MS_09d, d=MS_09da, y=3)



# combine all MASS data into one dataframe and export an excel -------------------

MA_SS_Sum_Data <- rbind(CE_B7_B,
                        CE_B7_C,
                        CE_B7_D,
                        CE_BA3,
                        CE_BP20,
                        CE_SWB_E1,
                        CE_SWB_N,
                        CE_SWB_S,
                        CE_Wheelock,
                        Mashpee_B1,
                        Mashpee_C,
                        Mashpee_G3,
                        MS_07a,
                        MS_09d)

write_xlsx(MA_SS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Summary_Data_SS_MA.xlsx")

# remove combined data sets to keep envr clean -------------------

rm(CE_B7_B,
   CE_B7_C,
   CE_B7_D,
   CE_BA3,
   CE_BP20,
   CE_SWB_E1,
   CE_SWB_N,
   CE_SWB_S,
   CE_Wheelock,
   Mashpee_B1,
   Mashpee_C,
   Mashpee_G3,
   MS_07a,
   MS_09d,
   CE_B7_Ba,
   CE_B7_Ca,
   CE_B7_Da,
   CE_BA3a,
   CE_BP20a,
   CE_SWB_E1a,
   CE_SWB_Na,
   CE_SWB_Sa,
   CE_Wheelocka,
   Mashpee_B1a,
   Mashpee_Ca,
   Mashpee_G3a,
   MS_07aa,
   MS_09da)


# calculate site means by treat category; -------------------

MA_SS_Control <- spec_summarySS(d=MA_SS_Sum_Data, x="Control", y=2)
MA_SS_Control$Region <- "MA"
MA_SS_Control$Treat_Type <- "Control"
MA_SS_Control <- reorderSS(MA_SS_Control)


MA_SS_FallRx <- spec_summarySS(d=MA_SS_Sum_Data, x="FallRx", y=3)
MA_SS_FallRx$Region <- "MA"
MA_SS_FallRx$Treat_Type <- "FallRx"
MA_SS_FallRx <- reorderSS(MA_SS_FallRx)


MA_SS_Harvest <- spec_summarySS(d=MA_SS_Sum_Data, x="Harvest", y=3)
MA_SS_Harvest$Region <- "MA"
MA_SS_Harvest$Treat_Type <- "Harvest"
MA_SS_Harvest <- reorderSS(MA_SS_Harvest)


MA_SS_MowRx <- spec_summarySS(d=MA_SS_Sum_Data, x="MowRx", y=3)
MA_SS_MowRx$Region <- "MA"
MA_SS_MowRx$Treat_Type <- "MowRx"
MA_SS_MowRx <- reorderSS(MA_SS_MowRx)


MA_SS_SpringRx <- spec_summarySS(d=MA_SS_Sum_Data, x="SpringRx", y=3)
MA_SS_SpringRx$Region <- "MA"
MA_SS_SpringRx$Treat_Type <- "SpringRx"
MA_SS_SpringRx <- reorderSS(MA_SS_SpringRx)



# now binding them all together -------------------

# Control data -------------------
Small_seedling_control_data <- rbind(ALB_SS_Control,
                                     LI_SS_Control,
                                     NH_SS_Control,
                                     MA_SS_Control,
                                     ME_SS_Control)


# and then finding an overall average for each treament category that i'll put into a excel later -------------------

SS_control_overall <- overall_summarySS(x=Small_seedling_control_data, y=5)
SS_control_overall$Treat_Type <- "Control"


# MowRx data -------------------
Small_seedling_mowrx_data <- rbind(ALB_SS_Sp_MowRx, 
                                   ALB_SS_Su_MowRx, 
                                   MA_SS_MowRx, 
                                   NH_SS_MowRx)

SS_mowrx_overall <- overall_summarySS(Small_seedling_mowrx_data, y=4)
SS_mowrx_overall$Treat_Type <- "MowRx"


# SpringRx data -------------------
Small_seedling_springrx_data <- rbind(ALB_SS_SpringRx, 
                                      LI_SS_SpringRx, 
                                      MA_SS_SpringRx)


SS_springrx_overall <- overall_summarySS(Small_seedling_springrx_data, y=3)
SS_springrx_overall$Treat_Type <- "SpringRx"


# FallRx data -------------------

Small_seedling_fallrx_data <- rbind(MA_SS_FallRx,
                                    ME_SS_FallRx,
                                    NH_SS_FallRx)

SS_fallrx_overall <- overall_summarySS(Small_seedling_fallrx_data, y=3)
SS_fallrx_overall$Treat_Type <- "FallRx"

# Harvest data -------------------

Small_seedling_harvest_data <- rbind(LI_SS_Harvest,
                                     MA_SS_Harvest)

SS_harvest_overall <- overall_summarySS(Small_seedling_harvest_data, y=2)
SS_harvest_overall$Treat_Type <- "Harvest"


# SPB data -------------------

Small_seedling_SPB_data <- LI_SS_SPB

SS_SPB_overall <- overall_summarySS(Small_seedling_SPB_data, y=1)
SS_SPB_overall$Treat_Type <- "SPB"



# now one excel to rule them all -------------------

SS_Summary <- rbind(SS_control_overall,
                    SS_fallrx_overall,
                    SS_mowrx_overall,
                    SS_springrx_overall,
                    SS_harvest_overall,
                    SS_SPB_overall)


write_xlsx(SS_Summary, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Overall_SS_FinalSummary.xlsx")



# I'd like to have all the regional data as well in excel sheets -------------------


Control_data_SS <- rbind(MA_SS_Control, 
                         LI_SS_Control, 
                         NH_SS_Control, 
                         ME_SS_Control, 
                         ALB_SS_Control)


write_xlsx(Control_data_SS, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx")



FallRx_data_SS <- rbind(ME_SS_FallRx,
                        NH_SS_FallRx,
                        MA_SS_FallRx)

AllRegionsSS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsSS, sheetName = "FallRx")
writeData(wb=AllRegionsSS, sheet="FallRx", x=FallRx_data_SS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsSS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx", overwrite = TRUE)



Harvest_data_SS <- rbind(LI_SS_Harvest,
                         MA_SS_Harvest)

AllRegionsSS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsSS, sheetName = "Harvest")
writeData(wb=AllRegionsSS, sheet="Harvest", x=Harvest_data_SS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsSS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx", overwrite = TRUE)



MowRx_data_SS <- rbind(NH_SS_MowRx,
                       ALB_SS_Sp_MowRx,
                       ALB_SS_Su_MowRx,
                       MA_SS_MowRx)

AllRegionsSS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsSS, sheetName = "MowRx")
writeData(wb=AllRegionsSS, sheet="MowRx", x=MowRx_data_SS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsSS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx", overwrite = TRUE)



SpringRx_data_SS <- rbind(LI_SS_SpringRx,
                          ALB_SS_SpringRx,
                          MA_SS_SpringRx)

AllRegionsSS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsSS, sheetName = "SpringRx")
writeData(wb=AllRegionsSS, sheet="SpringRx", x=SpringRx_data_SS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsSS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx", overwrite = TRUE)



#SPB - only one region
AllRegionsSS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsSS, sheetName = "SPB")
writeData(wb=AllRegionsSS, sheet="SPB", x=LI_SS_SPB, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsSS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/SSData/Allregions_SS_by_TreatType.xlsx", overwrite = TRUE)


























  
  
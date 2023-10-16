# Stutzman
# Pitch Pine Regeneration Project : Large Seedling code
#  2023-10-04

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)

# source functions -------------------
source("Functions/LS_plotsummary.R")
source("Functions/SSfilter.R")
source("Functions/site_region_treat.R")
source("Functions/LS_specsummary.R")
source("Functions/reorderSS.R")
source("Functions/LS_overallsummary.R")

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
LS_No_Treatment_SPB.csv4$Treat_Type <- "SPB"
LS_Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
LS_Thinned_Data.csv6$Treat_Type <- "Harvest"

# merge into one data set -------------------

large_seedling <- rbind(LS_Control_Data.csv1, 
                        LS_Fall_Burn_Data.csv2, 
                        LS_Mow_Burn_Data.csv3, 
                        LS_No_Treatment_SPB.csv4, 
                        LS_Spring_Burn_Data.csv5, 
                        LS_Thinned_Data.csv6)

# remove merged datasets to keep the envr clean -------------------

rm(LS_Control_Data.csv1, 
   LS_Fall_Burn_Data.csv2, 
   LS_Mow_Burn_Data.csv3, 
   LS_No_Treatment_SPB.csv4, 
   LS_Spring_Burn_Data.csv5, 
   LS_Thinned_Data.csv6)

# need to get plot numbers for each site -------------------
large_seedling <- large_seedling %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot_No))


# change species groupings -------------------

#these are the new groups:

# ACRU	
# PIRI	
# PR SP	:: PRSE, PRVI, PRPE
# Shrub Oaks ::	QUIL, QUPR
# Tree Oak ::	QUCO, QUAL, QUVE, QURU, QUPA
# OTHER HARDWOOD ::	SAAL, AMPS, BELE, BEPO, BENI, PODE, POGR, POTR, ACPE, FRAM, SAHU, FAGR, MASP, ACNE, CRSP, FRAL, ROPS, RHCA, ELUM
# OTHER SOFTWOOD ::	PIST, ABBA

large_seedling$Species_Groups <- NA

# ACRU
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code == 'ACRU', 'ACRU', large_seedling$Species_Groups)

# PIRI
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code == 'PIRI', 'PIRI', large_seedling$Species_Groups)

# PRSP	:: PRSE, PRVI, PRPE
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('PRSE', 'PRVI', 'PRPE'), 'PRSP', large_seedling$Species_Groups)

# Shrub Oaks ::	QUIL, QUPR
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('QUIL', 'QUPR'), 'Shrub_Oak', large_seedling$Species_Groups)

# Tree Oak ::	QUCO, QUAL, QUVE, QURU, QUPA
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('QUCO', 'QUAL', 'QUVE', 'QURU', 'QUPA'), 'Tree_Oak', large_seedling$Species_Groups)

# OTHER HARDWOOD ::	SAAL, AMPS, BELE, BEPO, BENI, PODE, POGR, POTR, ACPE, FRAM, SAHU, FAGR, MASP, ACNE, CRSP, FRAL, ROPS, RHCA, ELUM
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('SAAL', 'AMSP', 'BELE', 'BEPO', 'BENI', 'PODE', 'POGR', 'POTR', 'ACPE', 'FRAM', 'SAHU', 'FAGR', 'MASP', 'ACNE', 'CRSP', 'FRAL', 'ROPS', 'RHCA', 'ELUM', 'FRPE'), 'Other_Hardwood', large_seedling$Species_Groups)

# OTHER SOFTWOOD ::	PIST, ABBA
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('PIST', 'ABBA'), 'Other_Softwood', large_seedling$Species_Groups)


# now to check that all species have been assigned and only NAs are NAs in new column -------------------

check_ls <- large_seedling %>% 
  filter(if_any(Species_Groups, is.na))

# remove NAs -------------------

large_seedlingNA <- large_seedling[complete.cases(large_seedling),]

# removed datasets to keep envr neat -------------------
rm(check_ls,
   large_seedling)

# sort by region -------------------

MA_LS <- large_seedlingNA %>% 
  filter(Region == 'MA')

LI_LS <- large_seedlingNA %>% 
  filter(Region == 'LI')

ALB_LS <- large_seedlingNA %>% 
  filter(Region == 'ALB')

NH_LS <- large_seedlingNA %>% 
  filter(Region == 'NH')

ME_LS <- large_seedlingNA %>% 
  filter(Region == 'ME')

# remove dataset -------------------

rm(large_seedlingNA)

# start with MAINE data -------------------
TNCW_14a <- SSfilter(x=ME_LS, y="TNCW_14")
TNCW_14 <- LS_plotsummary(d=ME_LS, x="TNCW_14")
TNCW_14 <- site_region_treat(x=TNCW_14, d=TNCW_14a, y=3)

TNCW_12a <- SSfilter(x=ME_LS, y="TNCW_12")
TNCW_12 <- LS_plotsummary(d=ME_LS, x="TNCW_12")
TNCW_12 <- site_region_treat(x=TNCW_12, d=TNCW_12a, y=5)

TNCW_7SWa <- SSfilter(x=ME_LS, y="TNCW_7SW")
TNCW_7SW <- LS_plotsummary(d=ME_LS, x="TNCW_7SW")
TNCW_7SW <- site_region_treat(x=TNCW_7SW, d=TNCW_7SWa, y=3)

TNCW_18a <- SSfilter(x=ME_LS, y="TNCW_18")
TNCW_18 <- LS_plotsummary(d=ME_LS, x="TNCW_18")
TNCW_18 <- site_region_treat(x=TNCW_18, d=TNCW_18a, y=2)

TNCW_10SEa <- SSfilter(x=ME_LS, y="TNCW_10SE")
TNCW_10SE <- LS_plotsummary(d=ME_LS, x="TNCW_10SE")
TNCW_10SE <- site_region_treat(x=TNCW_10SE, d=TNCW_10SEa, y=3)

# combine Maine data into one dataframe and export to an excel -------------------

ME_LS_Sum_Data <- rbind(TNCW_10SE,
                        TNCW_12,
                        TNCW_14,
                        TNCW_18,
                        TNCW_7SW)

write_xlsx(ME_LS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Summary_Data_LS_ME.xlsx")

# remove superfluous data sets to keep things neat ------------------- if I need to have LS and SS running at the same time, I'll have to keep an eye on this

rm(TNCW_10SE,
   TNCW_12,
   TNCW_14,
   TNCW_18,
   TNCW_7SW,
   TNCW_10SEa,
   TNCW_12a,
   TNCW_14a,
   TNCW_18a,
   TNCW_7SWa)

# calculate site means by treat category -------------------

ME_LS_FallRx <- LS_specsummary(d=ME_LS_Sum_Data, x="FallRx", y=3)
ME_LS_FallRx$Region <- "ME"
ME_LS_FallRx$Treat_Type <- "FallRx"
ME_LS_FallRx <- reorderSS(x=ME_LS_FallRx)


ME_LS_Control <- LS_specsummary(d=ME_LS_Sum_Data, x= "Control", y=2)
ME_LS_Control$Region <- "ME"
ME_LS_Control$Treat_Type <- "Control"
ME_LS_Control <- reorderSS(ME_LS_Control)



# NEW HAMPSHIRE DATA -------------------
TNCO_Zito2a <- SSfilter(x=NH_LS, y="TNCO_Zito2")
TNCO_Zito2 <- LS_plotsummary(d=NH_LS, x="TNCO_Zito2")
TNCO_Zito2 <- site_region_treat(x=TNCO_Zito2, d=TNCO_Zito2a, y=4)

TNCO_WB10a <- SSfilter(x=NH_LS, y="TNCO_WB10")
TNCO_WB10 <- LS_plotsummary(d=NH_LS, x="TNCO_WB10")
TNCO_WB10 <- site_region_treat(x=TNCO_WB10, d=TNCO_WB10a, y=3)

TNCO_B1a <- SSfilter(x=NH_LS, y="TNCO_B1")
TNCO_B1 <- LS_plotsummary(d=NH_LS, x="TNCO_B1")
TNCO_B1 <- site_region_treat(x=TNCO_B1, d=TNCO_B1a, y=5)

TNCO_Hobbs5a <- SSfilter(x=NH_LS, y="TNCO_Hobbs5")
TNCO_Hobbs5 <- LS_plotsummary(d=NH_LS, x="TNCO_Hobbs5")
TNCO_Hobbs5 <- site_region_treat(x=TNCO_Hobbs5, d=TNCO_Hobbs5a, y=6)

TNCO_ESDB2a <- SSfilter(x=NH_LS, y="TNCO_ESDB2")
TNCO_ESDB2 <- LS_plotsummary(d=NH_LS, x="TNCO_ESDB2")
TNCO_ESDB2 <- site_region_treat(x=TNCO_ESDB2, d=TNCO_ESDB2a, y=3)

TNCO_WB1a <- SSfilter(x=NH_LS, y="TNCO_WB1")
TNCO_WB1 <- LS_plotsummary(d=NH_LS, x="TNCO_WB1")
TNCO_WB1 <- site_region_treat(x=TNCO_WB1, d=TNCO_WB1a, y=5)

TNCO_ESDB1a <- SSfilter(x=NH_LS, y="TNCO_ESDB1")
TNCO_ESDB1 <- LS_plotsummary(d=NH_LS, x="TNCO_ESDB1")
TNCO_ESDB1 <- site_region_treat(x=TNCO_ESDB1, d=TNCO_ESDB1a, y=5)

SPNHF_Harmona <- SSfilter(x=NH_LS, y="SPNHF_Harmon")
SPNHF_Harmon <- LS_plotsummary(d=NH_LS, x="SPNHF_Harmon")
SPNHF_Harmon <- site_region_treat(x=SPNHF_Harmon, d=SPNHF_Harmona, y=3)

# combine New Hampshire data into one dataframe and export to an excel -------------------

NH_LS_Sum_Data <- rbind(TNCO_B1, 
                        TNCO_ESDB1, 
                        TNCO_ESDB2, 
                        TNCO_Hobbs5, 
                        TNCO_WB1, 
                        TNCO_WB10, 
                        TNCO_Zito2,
                        SPNHF_Harmon)

write_xlsx(NH_LS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Summary_Data_LS_NH.xlsx")

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

NH_LS_FallRx <- LS_specsummary(d=NH_LS_Sum_Data, x="FallRx", y=3)
NH_LS_FallRx$Region <- "NH"
NH_LS_FallRx$Treat_Type <- "FallRx"
NH_LS_FallRx <- reorderSS(x=ME_LS_FallRx)

NH_LS_Control <- LS_specsummary(d=NH_LS_Sum_Data, x="Control", y=2)
NH_LS_Control$Region <- "NH"
NH_LS_Control$Treat_Type <- "FallRx"
NH_LS_Control <- reorderSS(NH_LS_Control)

NH_LS_MowRx <-LS_specsummary(d=NH_LS_Sum_Data, x="MowRx", y=3)
NH_LS_MowRx$Region <- "NH"
NH_LS_MowRx$Treat_Type <- "MowRX"
NH_LS_MowRx <- reorderSS(NH_LS_MowRx)



# LONG ISLAND DATA -------------------

RPD2_B1a <- SSfilter(x=LI_LS, y="RPD2_B1")
RPD2_B1 <- LS_plotsummary(d=LI_LS, x="RPD2_B1")
RPD2_B1 <- site_region_treat(x=RPD2_B1, d=RPD2_B1a, y=2)

RPD2_B2a <- SSfilter(x=LI_LS, y="RPD2_B2")
RPD2_B2 <- LS_plotsummary(d=LI_LS, x="RPD2_B2")
RPD2_B2 <- site_region_treat(x=RPD2_B2, d=RPD2_B2a, y=2)

RPD2_B3a <- SSfilter(x=LI_LS, y="RPD2_B3")
RPD2_B3 <- LS_plotsummary(d=LI_LS, x="RPD2_B3")
RPD2_B3 <- site_region_treat(x=RPD2_B3, d=RPD2_B3a, y=3)

RPD2_B4a <- SSfilter(x=LI_LS, y="RPD2_B4")
RPD2_B4 <- LS_plotsummary(d=LI_LS, x="RPD2_B4")
RPD2_B4 <- site_region_treat(x=RPD2_B4, d=RPD2_B4a, y=3)

RPD2_B5a <- SSfilter(x=LI_LS, y="RPD2_B5")
RPD2_B5 <- LS_plotsummary(d=LI_LS, x="RPD2_B5")
RPD2_B5 <- site_region_treat(x=RPD2_B5, d=RPD2_B5a, y=2)

RPD2_B6a <- SSfilter(x=LI_LS, y="RPD2_B6")
RPD2_B6 <- LS_plotsummary(d=LI_LS, x="RPD2_B6")
RPD2_B6 <- site_region_treat(x=RPD2_B6, d=RPD2_B6a, y=3)

RPD2_B8a <- SSfilter(x=LI_LS, y="RPD2_B8")
RPD2_B8 <- LS_plotsummary(d=LI_LS, x="RPD2_B8")
RPD2_B8 <- site_region_treat(x=RPD2_B8, d=RPD2_B8a, y=2)

RPD2_B7a <- SSfilter(x=LI_LS, y="RPD2_B7")
RPD2_B7 <- LS_plotsummary(d=LI_LS, x="RPD2_B7")
RPD2_B7 <- site_region_treat(x=RPD2_B7, d=RPD2_B7a, y=1)

RPD2_B9a <- SSfilter(x=LI_LS, y="RPD2_B9")
RPD2_B9 <- LS_plotsummary(d=LI_LS, x="RPD2_B9")
RPD2_B9 <- site_region_treat(x=RPD2_B9, d=RPD2_B9a, y=2)

HCP_GIa <- SSfilter(x=LI_LS, y="HCP_GI")
HCP_GI <- LS_plotsummary(d=LI_LS, x="HCP_GI")
HCP_GI <- site_region_treat(x=HCP_GI, d=HCP_GIa, y=1)

# SHCP_1a <- SSfilter(x=LI_LS, y="SHCP_1")
# SHCP_1 <- LS_plotsummary(d=LI_LS, x="SHCP_1")
# SHCP_1 <- site_region_treat(x=SHCP_1, d=SHCP_1a, y=0)
# there are no sapling at shcp, so it is omitted -------------------

WNWR_BTa <- SSfilter(x=LI_LS, y="WNWR_BT")
WNWR_BT <- LS_plotsummary(d=LI_LS, x="WNWR_BT")
WNWR_BT <- site_region_treat(x=WNWR_BT, d=WNWR_BTa, y=3)

# combine Long Island data into one dataframe and export to an excel -------------------

LI_LS_Sum_Data <- rbind(RPD2_B1,
                        RPD2_B2,
                        RPD2_B3,
                        RPD2_B4,
                        RPD2_B5,
                        RPD2_B6,
                        RPD2_B7,
                        RPD2_B9,
                        RPD2_B8,
                        HCP_GI,
                        WNWR_BT)

write_xlsx(LI_LS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Summary_Data_LS_LI.xlsx")

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
   HCP_GIa,
   WNWR_BTa)

# calculate site means by treat category -------------------
LI_LS_Control <- LS_specsummary(d=LI_LS_Sum_Data, x="Control", y=3)
LI_LS_Control$Region <- "LI"
LI_LS_Control$Treat_Type <- "Control"
LI_LS_Control <- reorderSS(LI_LS_Control)

LI_LS_SPB <- LS_specsummary(d=LI_LS_Sum_Data, x="SPB", y=3)
LI_LS_SPB$Region <- "LI"
LI_LS_SPB$Treat_Type <- "SPB"
LI_LS_SPB <- reorderSS(LI_LS_SPB)

LI_LS_SpringRx <- LS_specsummary(d=LI_LS_Sum_Data, x="SpringRx", y=3)
LI_LS_SpringRx$Region <- "LI"
LI_LS_SpringRx$Treat_Type <- "SpringRx"
LI_LS_SpringRx <- reorderSS(LI_LS_SpringRx)

LI_LS_Harvest <- LS_specsummary(d=LI_LS_Sum_Data, x="Harvest", y=3)
LI_LS_Harvest$Region <- "LI"
LI_LS_Harvest$Treat_Type <- "Harvest"
LI_LS_Harvest <- reorderSS(LI_LS_Harvest)




# Now for Albany Data -------------------
APB_Alleycata <- SSfilter(x=ALB_LS, y="APB_Alleycat")
APB_Alleycat <- LS_plotsummary(d=ALB_LS, x="APB_Alleycat")
APB_Alleycat <- site_region_treat(x=APB_Alleycat, d=APB_Alleycata, y=4)

APB_Bivya <- SSfilter(x=ALB_LS, y="APB_Bivy")
APB_Bivy <- LS_plotsummary(d=ALB_LS, x="APB_Bivy")
APB_Bivy <- site_region_treat(x=APB_Bivy, d=APB_Bivya, y=4)

APB_Chubb_Ea <- SSfilter(x=ALB_LS, y="APB_Chubb_E")
APB_Chubb_E <- LS_plotsummary(d=ALB_LS, x="APB_Chubb_E")
APB_Chubb_E <- site_region_treat(x=APB_Chubb_E, d=APB_Chubb_Ea, y=5)

APB_Chubb_Wa <- SSfilter(x=ALB_LS, y="APB_Chubb_W")
APB_Chubb_W <- LS_plotsummary(d=ALB_LS, x="APB_Chubb_W")
APB_Chubb_W <- site_region_treat(x=APB_Chubb_W, d=APB_Chubb_Wa, y=5)

APB_Dandya <- SSfilter(x=ALB_LS, y="APB_Dandy")
APB_Dandy <- LS_plotsummary(d=ALB_LS, x="APB_Dandy")
APB_Dandy<- site_region_treat(x=APB_Dandy, d=APB_Dandya, y=5)

APB_Fowlersa <- SSfilter(x=ALB_LS, y="APB_Fowlers")
APB_Fowlers <- LS_plotsummary(d=ALB_LS, x="APB_Fowlers")
APB_Fowlers <- site_region_treat(x=APB_Fowlers, d=APB_Fowlersa, y=6)

APB_Hippoa <- SSfilter(x=ALB_LS, y="APB_Hippo")
APB_Hippo <- LS_plotsummary(d=ALB_LS, x="APB_Hippo")
APB_Hippo <- site_region_treat(x=APB_Hippo, d=APB_Hippoa, y=4)

APB_Hoffmana <- SSfilter(x=ALB_LS, y="APB_Hoffman")
APB_Hoffman <- LS_plotsummary(d=ALB_LS, x="APB_Hoffman")
APB_Hoffman <- site_region_treat(x=APB_Hoffman, d=APB_Hoffmana, y=6)

APB_HuckMounda <- SSfilter(x=ALB_LS, y="APB_HuckMound")
APB_HuckMound <- LS_plotsummary(d=ALB_LS, x="APB_HuckMound")
APB_HuckMound <- site_region_treat(x=APB_HuckMound, d=APB_HuckMounda, y=4)

APB_Humdingera <- SSfilter(x=ALB_LS, y="APB_Humdinger")
APB_Humdinger <- LS_plotsummary(d=ALB_LS, x="APB_Humdinger")
APB_Humdinger <- site_region_treat(x=APB_Humdinger,
                                   d=APB_Humdingera, y=5)

APB_Hydroa <- SSfilter(x=ALB_LS, y="APB_Hydro")
APB_Hydro <- LS_plotsummary(d=ALB_LS, x="APB_Hydro")
APB_Hydro <- site_region_treat(x=APB_Hydro, d=APB_Hydroa, y=4)

# combine all Albany data into one dataframe and export an excel -------------------

ALB_LS_Sum_Data <- rbind(APB_Hippo,
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

write_xlsx(ALB_LS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Summary_Data_LS_ALB.xlsx")

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

ALB_LS_Control <- LS_specsummary(d=ALB_LS_Sum_Data, x="Control", y=2)
ALB_LS_Control$Region <- "ALB"
ALB_LS_Control$Treat_Type <- "Control"
ALB_LS_Control <- reorderSS(ALB_LS_Control)

ALB_LS_SpringRx <- LS_specsummary(d=ALB_LS_Sum_Data, x="SpringRx", y=3)
ALB_LS_SpringRx$Region <- "ALB"
ALB_LS_SpringRx$Treat_Type <- "SpringRx"
ALB_LS_SpringRx <- reorderSS(ALB_LS_SpringRx)

ALB_LS_Sp_MowRx <- ALB_LS_Sum_Data %>% 
  filter(Site %in% c("APB_Humdinger", "APB_Bivy", "APB_Alleycat")) %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), 
            T_Prop_Browse = ((sum(Prop_Browse)/3)), 
            T_Prop_SS= ((sum(Prop_SS)/3)),
            No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*1000)
ALB_LS_Sp_MowRx$Region <- "ALB"
ALB_LS_Sp_MowRx$Treat_Type <- "MowRx"
ALB_LS_Sp_MowRx <- reorderSS(ALB_LS_Sp_MowRx)

ALB_LS_Su_MowRx <- ALB_LS_Sum_Data %>% 
  filter(Site %in% c("APB_Chubb_W", "APB_Chubb_E", "APB_Dandy")) %>% 
  group_by(Species_Groups) %>% 
  summarise(Species_Total = sum(Mean_Total), 
            T_Prop_Browse = ((sum(Prop_Browse)/3)), 
            T_Prop_SS= ((sum(Prop_SS)/3)),
            No_of_Sites = n_distinct(Site)) %>% 
  dplyr::mutate(Group_Mean = Species_Total/3) %>% 
  dplyr::mutate(Per_HA = Group_Mean*1000)
ALB_LS_Su_MowRx$Region <- "ALB"
ALB_LS_Su_MowRx$Treat_Type <- "MowRx"
ALB_LS_Su_MowRx <- reorderSS(x=ALB_LS_Su_MowRx)




# Now for MASS Data -------------------

CE_B7_Ba <- SSfilter(x=MA_LS, y="CE_B7_B")
CE_B7_B <- LS_plotsummary(d=MA_LS, x="CE_B7_B")
CE_B7_B <- site_region_treat(x=CE_B7_B, d=CE_B7_Ba, y=5)

CE_B7_Ca <- SSfilter(x=MA_LS, y="CE_B7_C")
CE_B7_C <- LS_plotsummary(d=MA_LS, x="CE_B7_C")
CE_B7_C <- site_region_treat(x=CE_B7_C, d=CE_B7_Ca, y=5)

CE_B7_Da <- SSfilter(x=MA_LS, y="CE_B7_D")
CE_B7_D <- LS_plotsummary(d=MA_LS, x="CE_B7_D")
CE_B7_D <- site_region_treat(x=CE_B7_D, d=CE_B7_Da, y=5)

CE_BA3a <- SSfilter(x=MA_LS, y="CE_BA3")
CE_BA3 <- LS_plotsummary(d=MA_LS, x="CE_BA3")
CE_BA3 <- site_region_treat(x=CE_BA3, d=CE_BA3a, y=4)

CE_BP20a <- SSfilter(x=MA_LS, y="CE_BP20")
CE_BP20 <- LS_plotsummary(d=MA_LS, x="CE_BP20")
CE_BP20 <- site_region_treat(x=CE_BP20, d=CE_BP20a, y=4)

CE_SWB_E1a <- SSfilter(x=MA_LS, y="CE_SWB_E1")
CE_SWB_E1 <- LS_plotsummary(d=MA_LS, x="CE_SWB_E1")
CE_SWB_E1 <- site_region_treat(x=CE_SWB_E1, d=CE_SWB_E1a, y=3)

CE_SWB_Na <- SSfilter(x=MA_LS, y="CE_SWB_N")
CE_SWB_N <- LS_plotsummary(d=MA_LS, x="CE_SWB_N")
CE_SWB_N <- site_region_treat(x=CE_SWB_N, d=CE_SWB_Na, y=3)

CE_SWB_Sa <- SSfilter(x=MA_LS, y="CE_SWB_S")
CE_SWB_S <- LS_plotsummary(d=MA_LS, x="CE_SWB_S")
CE_SWB_S <- site_region_treat(x=CE_SWB_S, d=CE_SWB_Sa, y=3)

CE_Wheelocka <- SSfilter(x=MA_LS, y="CE_Wheelock")
CE_Wheelock <- LS_plotsummary(d=MA_LS, x="CE_Wheelock")
CE_Wheelock <- site_region_treat(x=CE_Wheelock, d=CE_Wheelocka, y=6)

Mashpee_B1a <- SSfilter(x=MA_LS, y="Mashpee_B1")
Mashpee_B1 <- LS_plotsummary(d=MA_LS, x="Mashpee_B1")
Mashpee_B1 <- site_region_treat(x=Mashpee_B1, d=Mashpee_B1a, y=5)

Mashpee_Ca <- SSfilter(x=MA_LS, y="Mashpee_C")
Mashpee_C <- LS_plotsummary(d=MA_LS, x="Mashpee_C")
Mashpee_C <- site_region_treat(x=Mashpee_C, d=Mashpee_Ca, y=2)

Mashpee_G3a <- SSfilter(x=MA_LS, y="Mashpee_G3")
Mashpee_G3 <- LS_plotsummary(d=MA_LS, x="Mashpee_G3")
Mashpee_G3 <- site_region_treat(x=Mashpee_G3, d=Mashpee_G3a, y=2)

MS_07aa <- SSfilter(x=MA_LS, y="MS_07a")
MS_07a <- LS_plotsummary(d=MA_LS, x="MS_07a")
MS_07a <- site_region_treat(x=MS_07a, d=MS_07aa, y=4)

MS_09da <- SSfilter(x=MA_LS, y="MS_09d")
MS_09d <- LS_plotsummary(d=MA_LS, x="MS_09d")
MS_09d <- site_region_treat(x=MS_09d, d=MS_09da, y=2)


# combine all MASS data into one dataframe and export an excel -------------------

MA_LS_Sum_Data <- rbind(CE_B7_B,
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

write_xlsx(MA_LS_Sum_Data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Summary_Data_LS_MA.xlsx")

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
MA_LS_Control <- LS_specsummary(d=MA_LS_Sum_Data, x = "Control", y=2)
MA_LS_Control$Region <- "MA"
MA_LS_Control$Treat_Type <- "Control"
MA_LS_Control <- reorderSS(MA_LS_Control)

MA_LS_FallRx <- LS_specsummary(d=MA_LS_Sum_Data, x="FallRx", y=3)
MA_LS_FallRx$Region <- "MA"
MA_LS_FallRx$Treat_Type <- "FallRx"
MA_LS_FallRx <- reorderSS(MA_LS_FallRx)

MA_LS_Harvest <- LS_specsummary(d=MA_LS_Sum_Data, x="Harvest", y=3)
MA_LS_Harvest$Region <- "MA"
MA_LS_Harvest$Treat_Type <- "Harvest"
MA_LS_Harvest <- reorderSS(MA_LS_Harvest)

MA_LS_MowRx <- LS_specsummary(d=MA_LS_Sum_Data, x="MowRx", y=3)
MA_LS_MowRx$Region <- "MA"
MA_LS_MowRx$Treat_Type <- "MowRx"
MA_LS_MowRx <- reorderSS(MA_LS_MowRx)

MA_LS_SpringRx <- LS_specsummary(d=MA_LS_Sum_Data, x="SpringRx", y=3)
MA_LS_SpringRx$Region <- "MA"
MA_LS_SpringRx$Treat_Type <- "SpringRx"
MA_LS_SpringRx <- reorderSS(MA_LS_SpringRx)



# now binding them all together -------------------

# Control data -------------------
Large_seedling_control_data <- rbind(ALB_LS_Control,
                                     LI_LS_Control,
                                     NH_LS_Control,
                                     MA_LS_Control,
                                     ME_LS_Control)


# control -------------------
LS_control_overall <- LS_overallsummary(x=Large_seedling_control_data, y=5)
LS_control_overall$Treat_Type <- "Control"


# MowRx data -------------------
Large_seedling_mowrx_data <- rbind(ALB_LS_Sp_MowRx, 
                                   ALB_LS_Su_MowRx, 
                                   MA_LS_MowRx, 
                                   NH_LS_MowRx)


LS_mowrx_overall <- LS_overallsummary(Large_seedling_mowrx_data, y=4)
LS_mowrx_overall$Treat_Type <- "MowRx"


# SpringRx data -------------------
Large_seedling_springrx_data <- rbind(ALB_LS_SpringRx, 
                                      LI_LS_SpringRx, 
                                      MA_LS_SpringRx)


LS_springrx_overall <- LS_overallsummary(Large_seedling_springrx_data, y=3)
LS_springrx_overall$Treat_Type <- "SpringRx"



# FallRx data -------------------

Large_seedling_fallrx_data <- rbind(MA_LS_FallRx,
                                    ME_LS_FallRx,
                                    NH_LS_FallRx)

LS_fallrx_overall <- LS_overallsummary(Large_seedling_fallrx_data, y=3)
LS_fallrx_overall$Treat_Type <- "FallRx"


# Harvest data -------------------

Large_seedling_harvest_data <- rbind(LI_LS_Harvest,
                                     MA_LS_Harvest)


LS_harvest_overall <- LS_overallsummary(Large_seedling_harvest_data, y=2)
LS_harvest_overall$Treat_Type <- "Harvest"



# SPB data -------------------

Large_seedling_SPB_data <- LI_LS_SPB

LS_SPB_overall <- LS_overallsummary(Large_seedling_SPB_data, y=1)
LS_SPB_overall$Treat_Type <- "SPB"



# now one excel to rule them all -------------------

LS_Summary <- rbind(LS_control_overall,
                    LS_fallrx_overall,
                    LS_mowrx_overall,
                    LS_springrx_overall,
                    LS_harvest_overall,
                    LS_SPB_overall)



write_xlsx(LS_Summary, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Overall_LS_FinalSummary.xlsx")


# I'd like to have all the regional data as well in excel sheets -------------------


Control_data_LS <- rbind(MA_LS_Control, 
                         LI_LS_Control, 
                         NH_LS_Control, 
                         ME_LS_Control, 
                         ALB_LS_Control)


write_xlsx(Control_data_LS, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx")

FallRx_data_LS <- rbind(ME_LS_FallRx,
                        NH_LS_FallRx,
                        MA_LS_FallRx)

AllRegionsLS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsLS, sheetName = "FallRx")
writeData(wb=AllRegionsLS, sheet="FallRx", x=FallRx_data_LS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsLS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx", overwrite = TRUE)


Harvest_data_LS <- rbind(LI_LS_Harvest,
                         MA_LS_Harvest)

AllRegionsLS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsLS, sheetName = "Harvest")
writeData(wb=AllRegionsLS, sheet="Harvest", x=Harvest_data_LS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsLS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx", overwrite = TRUE)


MowRx_data_LS <- rbind(NH_LS_MowRx,
                       ALB_LS_Sp_MowRx,
                       ALB_LS_Su_MowRx,
                       MA_LS_MowRx)

AllRegionsLS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsLS, sheetName = "MowRx")
writeData(wb=AllRegionsLS, sheet="MowRx", x=MowRx_data_LS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsLS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx", overwrite = TRUE)


SpringRx_data_LS <- rbind(LI_LS_SpringRx,
                          ALB_LS_SpringRx,
                          MA_LS_SpringRx)

AllRegionsLS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsLS, sheetName = "SpringRx")
writeData(wb=AllRegionsLS, sheet="SpringRx", x=SpringRx_data_LS, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsLS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx", overwrite = TRUE)


# SPB dtat -------------------

AllRegionsLS <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx")

addWorksheet(wb=AllRegionsLS, sheetName = "SPB")
writeData(wb=AllRegionsLS, sheet="SPB", x=LI_LS_SPB, startCol = 1, startRow = 1)

saveWorkbook(wb=AllRegionsLS, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/LSData/Allregions_LS_by_TreatType.xlsx", overwrite = TRUE)


















# get mean abundance and frequency to look at species -------------------

# x2 <- large_seedling %>% 
#   group_by(Latin_Name) %>% 
#   summarize(meanAbundance = mean(Total), number_of_occurrences=n())
# 
# # add new worksheet to existing file and save -------------------
# 
# wb1 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx")
# addWorksheet(wb=wb1, sheetName = "LargeSeedling")
# writeData(wb=wb1, sheet="LargeSeedling", x=x2, startCol = 1, startRow = 1)
# 
# saveWorkbook(wb=wb1, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/regen_specieslist.xlsx", overwrite = TRUE)
# 
# 
# # now trying the gross totals for this -------------------
# 
# ls3 <- large_seedling %>% 
#   group_by(Latin_Name) %>% 
#   summarize(SUM = sum(Total), number_of_occurrences=n()) %>% 
#   dplyr::mutate(T_Mean = SUM/1065) %>% 
#   dplyr::mutate(Per_HA = T_Mean*1000)
# 
# 
# # now to make this an excel -------------------
# 
# wb2 <- loadWorkbook("/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx")
# addWorksheet(wb=wb2, sheetName = "LargeSeedling")
# writeData(wb=wb2, sheet="LargeSeedling", x=ls3, startCol = 1, startRow = 1)
# 
# saveWorkbook(wb=wb2, file="/Users/user/Desktop/Data/Regen_RProj/DataObjects/specieslist.xlsx", overwrite = TRUE)






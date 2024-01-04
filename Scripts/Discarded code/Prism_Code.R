# Stutzman
# Pitch Pine Regen Project: Prism/Basal Area code
#  2023-10-13

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)

# functions -------------------
source("Functions/SSfilter.R")
source("Functions/prism_unitsummary.R")
source("Functions/site_region_treat_ba.R")
source("Functions/prism_treat_summary.R")
source("Functions/prism_reorder.R")
source("Functions/prism_overallsummary.R")


# global variables -------------------

file_folder_P <- "CleanData/prism_CSV/"

# body -------------------

file_names_P <- list.files(path=file_folder_P)


#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_P)) {
  assign(paste0(file_names_P[i], i),
         read.csv(paste0(file_folder_P, file_names_P[i])))
}

# adding treatment types -------------------

P_Control_Data.csv1$Treat_Type <- "Control"
P_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
P_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
P_No_Treatment_SPB.csv4$Treat_Type <- "SPB"
P_Spring_Burn_Data.csv5$Treat_Type <- "SpringRx"
P_Thinned_Data.csv6$Treat_Type <- "Harvest"

# merge into one data set -------------------

prism <- rbind(P_Control_Data.csv1,
               P_Fall_Burn_Data.csv2,
               P_Mow_Burn_Data.csv3,
               P_No_Treatment_SPB.csv4,
               P_Spring_Burn_Data.csv5,
               P_Thinned_Data.csv6)

# remove merged datasets to keep envr clean -------------------

rm(P_Control_Data.csv1,
   P_Fall_Burn_Data.csv2,
   P_Mow_Burn_Data.csv3,
   P_No_Treatment_SPB.csv4,
   P_Spring_Burn_Data.csv5,
   P_Thinned_Data.csv6)

# now I should find the number of plots per site and take a look at the species list to see how many I have and if I need to condense it into groups//similar to large seedling and small seedling -------------------


prism <- prism %>% 
  group_by(Treat_Type) %>% 
  group_by(Site) %>% 
  dplyr::mutate(No_of_Plots = n_distinct(Plot_No))

#ok, lets look at live basal area
prism_species <- prism %>% 
  group_by(Latin_Name) %>% 
  summarise(Total_Live_BA = sum(Live_Total))

#write_xlsx(prism_species, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/prism_specieslist.xlsx")


# lets check out dead BA -------------------
prism_dead <- prism %>% 
  group_by(Latin_Name) %>% 
  summarise(Total_Dead_BA = sum(Dead_Total))

#write_xlsx(prism_dead, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/prism_dead_specieslist.xlsx")



# going to move forward with the groups I've put out to analyze the data, but they are easy to come back in here and change -------------------


# live BA data -------------------

# PIRI	
# TREE_OAKS	:: QUCO, QUAL, QUVE, QUPA, QURU, QUST
# PIST	
# ACRU	
# OTHER:: BEPO, NYSY, QUIL, SAAL, FAGR, QUPR, BENI, BELE, FRPE, MORSP, PRSP, ULAM, ACPE, POGR, PODE, POTR, PIRU, TSCA, PRSE


prism_live <- prism %>% 
  filter(Live_Total >= 1)

prism_live$Species_Groups <- NA

# PIRI
prism_live$Species_Groups <- ifelse(prism_live$Species_Code == 'PIRI', 'PIRI', prism_live$Species_Groups)

# TREE_OAKS	:: QUCO, QUAL, QUVE, QUPA, QURU, QUST
prism_live$Species_Groups <- ifelse(prism_live$Species_Code %in% c('QUCO', 'QUAL', 'QUVE', 'QUPA', 'QURU', 'QUST'), 'QUSP', prism_live$Species_Groups)

# PIST
prism_live$Species_Groups <- ifelse(prism_live$Species_Code == 'PIST', 'PIST', prism_live$Species_Groups)

#ACRU
prism_live$Species_Groups <- ifelse(prism_live$Species_Code == 'ACRU', 'ACRU', prism_live$Species_Groups)


# OTHER_HARDWOODS	:: NYSY, QUIL, SAAL, FAGR, QUPR, BENI, BELE, FRPE, MORSP, PRSP, ULAM
prism_live$Species_Groups <- ifelse(prism_live$Species_Code %in% c('NYSY', 'QUIL', 'SAAL', 'FAGR', 'QUPR', 'BENI', 'BELE', 'FRPE', 'MOSP', 'PRSP', 'ULAM', 'ACPE','PRSE','POGR', 'PODE', 'POTR', 'BEPO', 'PIRU', 'TSCA'), 'Other', prism_live$Species_Groups)

# now to check -------------------

check_prism <- prism_live %>% 
  filter(if_any(Species_Groups, is.na))

rm(check_prism)

# sort by region -------------------

MA_prism <- prism_live %>% 
  filter(Region== 'MA')

LI_prism <- prism_live %>% 
  filter(Region== 'LI')

ALB_prism <- prism_live %>% 
  filter(Region== 'ALB')

ME_prism <- prism_live %>% 
  filter(Region== 'ME')

NH_prism <- prism_live %>% 
  filter(Region== 'NH')


# Maine data -------------------

TNCW_14a <- SSfilter(x=ME_prism, y="TNCW_14")
TNCW_14 <- prism_unitsummary(d=ME_prism, x="TNCW_14")
TNCW_14 <- site_region_treat_ba(x=TNCW_14, d=TNCW_14a, y=3)

TNCW_12a <- SSfilter(x=ME_prism, y="TNCW_12")
TNCW_12 <- prism_unitsummary(d=ME_prism, x="TNCW_12")
TNCW_12 <- site_region_treat_ba(x=TNCW_12, d=TNCW_12a, y=4)

TNCW_7SWa <- SSfilter(x=ME_prism, y="TNCW_7SW")
TNCW_7SW <- prism_unitsummary(d=ME_prism, x="TNCW_7SW")
TNCW_7SW <- site_region_treat_ba(x=TNCW_7SW, d=TNCW_7SWa, y=4)

TNCW_18a <- SSfilter(x=ME_prism, y="TNCW_18")
TNCW_18 <- prism_unitsummary(d=ME_prism, x="TNCW_18")
TNCW_18 <- site_region_treat_ba(x=TNCW_18, d=TNCW_18a, y=4)

TNCW_10SEa <- SSfilter(x=ME_prism, y="TNCW_10SE")
TNCW_10SE <- prism_unitsummary(d=ME_prism, x="TNCW_10SE")
TNCW_10SE <- site_region_treat_ba(x=TNCW_10SE, d=TNCW_10SEa, y=4)

# combine Maine data into one data frame and export -------------------

ME_prism_sum_data <- rbind(TNCW_10SE,
                            TNCW_12,
                            TNCW_14,
                            TNCW_18,
                            TNCW_7SW)

write_xlsx(ME_prism_sum_data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/Summary_Data_Prism_ME.xlsx")

# remove superfluous data sets to keep things neat -------------------

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

# for now I'm going to just do metric units and leave the english 
#out here -------------------

# Avg_BA_met =(sum(BA_sm_ha))/3
# dplyr::mutate(Mean_Site_BA_met = sum(Avg_BA_met)) %>% 
# dplyr::mutate(Mean_Site_BA_eng = round(sum(Avg_BA_eng), digits=0))  

ME_prism_FallRx <- prism_treat_summary(d=ME_prism_sum_data, x="FallRx", y=3)
ME_prism_FallRx$Region <- "ME"
ME_prism_FallRx$Treat_Type <- "FallRx"
ME_prism_FallRx <- prism_reorder(ME_prism_FallRx)

ME_prism_Control <- prism_treat_summary(d=ME_prism_sum_data, x="Control", y=2)
ME_prism_Control$Region <- "ME"
ME_prism_Control$Treat_Type <- "Control"
ME_prism_Control <- prism_reorder(ME_prism_Control)




# NH data -------------------

TNCO_Zito2a <- SSfilter(x=NH_prism, y="TNCO_Zito2")
TNCO_Zito2 <- prism_unitsummary(d=NH_prism, x="TNCO_Zito2")
TNCO_Zito2 <- site_region_treat_ba(x=TNCO_Zito2, d=TNCO_Zito2a, y=5)

TNCO_WB10a <- SSfilter(x=NH_prism, y="TNCO_WB10")
TNCO_WB10 <- prism_unitsummary(d=NH_prism, x="TNCO_WB10")
TNCO_WB10 <- site_region_treat_ba(x=TNCO_WB10, d=TNCO_WB10a, y=3)

TNCO_B1a <- SSfilter(x=NH_prism, y="TNCO_B1")
TNCO_B1 <- prism_unitsummary(d=NH_prism, x="TNCO_B1")
TNCO_B1 <- site_region_treat_ba(x=TNCO_B1, d=TNCO_B1a, y=3)

TNCO_Hobbs5a <- SSfilter(x=NH_prism, y="TNCO_Hobbs5")
TNCO_Hobbs5 <- prism_unitsummary(d=NH_prism, x="TNCO_Hobbs5")
TNCO_Hobbs5 <- site_region_treat_ba(x=TNCO_Hobbs5, d=TNCO_Hobbs5a, y=1)

TNCO_ESDB2a <- SSfilter(x=NH_prism, y="TNCO_ESDB2")
TNCO_ESDB2 <- prism_unitsummary(d=NH_prism, x="TNCO_ESDB2")
TNCO_ESDB2 <- site_region_treat_ba(x=TNCO_ESDB2, d=TNCO_ESDB2a, y=2)

TNCO_WB1a <- SSfilter(x=NH_prism, y="TNCO_WB1")
TNCO_WB1 <- prism_unitsummary(d=NH_prism, x="TNCO_WB1")
TNCO_WB1 <- site_region_treat_ba(x=TNCO_WB1, d=TNCO_WB1a, y=2)

TNCO_ESDB1a <- SSfilter(x=NH_prism, y="TNCO_ESDB1")
TNCO_ESDB1 <- prism_unitsummary(d=NH_prism, x="TNCO_ESDB1")
TNCO_ESDB1 <- site_region_treat_ba(x=TNCO_ESDB1, d=TNCO_ESDB1a, y=2)

SPNHF_Harmona <- SSfilter(x=NH_prism, y="SPNHF_Harmon")
SPNHF_Harmon <- prism_unitsummary(d=NH_prism, x="SPNHF_Harmon")
SPNHF_Harmon <- site_region_treat_ba(x=SPNHF_Harmon, d=SPNHF_Harmona, y=5)

# combine New Hampshire data into one dataframe & export

NH_prism_sum_data <- rbind(TNCO_B1, 
                           TNCO_ESDB1, 
                           TNCO_ESDB2, 
                           TNCO_Hobbs5, 
                           TNCO_WB1, 
                           TNCO_WB10, 
                           TNCO_Zito2,
                           SPNHF_Harmon)

write_xlsx(NH_prism_sum_data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/Summary_Data_Prism_NH.xlsx")

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

NH_prism_FallRx <- prism_treat_summary(d=NH_prism_sum_data, x="FallRx", y=3)
NH_prism_FallRx$Region <- "NH"
NH_prism_FallRx$Treat_Type <- "FallRx"
NH_prism_FallRx <- prism_reorder(NH_prism_FallRx)

NH_prism_Control <- prism_treat_summary(d=NH_prism_sum_data, x="Control", y=2)
NH_prism_Control$Region <- "NH"
NH_prism_Control$Treat_Type <- "Control"
NH_prism_Control <- prism_reorder(NH_prism_Control)

NH_prism_MowRx <- prism_treat_summary(d=NH_prism_sum_data, x="MowRx", y=3)
NH_prism_MowRx$Region <- "NH"
NH_prism_MowRx$Treat_Type <- "MowRx"
NH_prism_MowRx <- prism_reorder(NH_prism_MowRx)

# Long Island data -------------------

RPD2_B1a <- SSfilter(x=LI_prism, y="RPD2_B1")
RPD2_B1 <- prism_unitsummary(d=LI_prism, x="RPD2_B1")
RPD2_B1 <- site_region_treat_ba(x=RPD2_B1, d=RPD2_B1a, y=2)

RPD2_B2a <- SSfilter(x=LI_prism, y="RPD2_B2")
RPD2_B2 <- prism_unitsummary(d=LI_prism, x="RPD2_B2")
RPD2_B2 <- site_region_treat_ba(x=RPD2_B2, d=RPD2_B2a, y=2)

RPD2_B3a <- SSfilter(x=LI_prism, y="RPD2_B3")
RPD2_B3 <- prism_unitsummary(d=LI_prism, x="RPD2_B3")
RPD2_B3 <- site_region_treat_ba(x=RPD2_B3, d=RPD2_B3a, y=2)

RPD2_B4a <- SSfilter(x=LI_prism, y="RPD2_B4")
RPD2_B4 <- prism_unitsummary(d=LI_prism, x="RPD2_B4")
RPD2_B4 <- site_region_treat_ba(x=RPD2_B4, d=RPD2_B4a, y=2)

RPD2_B5a <- SSfilter(x=LI_prism, y="RPD2_B5")
RPD2_B5 <- prism_unitsummary(d=LI_prism, x="RPD2_B5")
RPD2_B5 <- site_region_treat_ba(x=RPD2_B5, d=RPD2_B5a, y=2)

RPD2_B6a <- SSfilter(x=LI_prism, y="RPD2_B6")
RPD2_B6 <- prism_unitsummary(d=LI_prism, x="RPD2_B6")
RPD2_B6 <- site_region_treat_ba(x=RPD2_B6, d=RPD2_B6a, y=2)

RPD2_B7a <- SSfilter(x=LI_prism, y="RPD2_B7")
RPD2_B7 <- prism_unitsummary(d=LI_prism, x="RPD2_B7")
RPD2_B7 <- site_region_treat_ba(x=RPD2_B7, d=RPD2_B7a, y=2)

RPD2_B8a <- SSfilter(x=LI_prism, y="RPD2_B8")
RPD2_B8 <- prism_unitsummary(d=LI_prism, x="RPD2_B8")
RPD2_B8 <- site_region_treat_ba(x=RPD2_B8, d=RPD2_B8a, y=2)

RPD2_B9a <- SSfilter(x=LI_prism, y="RPD2_B9")
RPD2_B9 <- prism_unitsummary(d=LI_prism, x="RPD2_B9")
RPD2_B9 <- site_region_treat_ba(x=RPD2_B9, d=RPD2_B9a, y=2)

HCP_GIa <- SSfilter(x=LI_prism, y="HCP_GI")
HCP_GI <- prism_unitsummary(d=LI_prism, x="HCP_GI")
HCP_GI <- site_region_treat_ba(x=HCP_GI, d=HCP_GIa, y=2)

SHCP_1a <- SSfilter(x=LI_prism, y="SHCP_1")
SHCP_1 <- prism_unitsummary(d=LI_prism, x="SHCP_1")
SHCP_1<- site_region_treat_ba(x=SHCP_1, d=SHCP_1a, y=4)

WNWR_BTa <- SSfilter(x=LI_prism, y="WNWR_BT")
WNWR_BT <- prism_unitsummary(d=LI_prism, x="WNWR_BT")
WNWR_BT <- site_region_treat_ba(x=WNWR_BT, d=WNWR_BTa, y=1)

# combine Long Island data into one dataframe and export to an excel -------------------

LI_prism_sum_data <- rbind(RPD2_B1,
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


write_xlsx(LI_prism_sum_data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/Summary_Data_Prism_LI.xlsx")

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
LI_prism_Control <- prism_treat_summary(d=LI_prism_sum_data, x="Control", y=3)
LI_prism_Control$Region <- "LI"
LI_prism_Control$Treat_Type <- "Control"
LI_prism_Control <- prism_reorder(LI_prism_Control)

LI_prism_SPB <- prism_treat_summary(d=LI_prism_sum_data, x="SPB", y=3)
LI_prism_SPB$Region <- "LI"
LI_prism_SPB$Treat_Type <- "SPB"
LI_prism_SPB <- prism_reorder(LI_prism_SPB)

LI_prism_SpringRx <- prism_treat_summary(d=LI_prism_sum_data, x="SpringRx", y=3)
LI_prism_SpringRx$Region <- "LI"
LI_prism_SpringRx$Treat_Type <- "SpringRx"
LI_prism_SpringRx <- prism_reorder(LI_prism_SpringRx)

LI_prism_Harvest <- prism_treat_summary(d=LI_prism_sum_data, x="Harvest", y=3)
LI_prism_Harvest$Region <- "LI"
LI_prism_Harvest$Treat_Type <- "Harvest"
LI_prism_Harvest <- prism_reorder(LI_prism_Harvest)



# Now for Albany Data -------------------

APB_Alleycata <- SSfilter(x=ALB_prism, y="APB_Alleycat")
APB_Alleycat <- prism_unitsummary(d=ALB_prism, x="APB_Alleycat")
APB_Alleycat <- site_region_treat_ba(x=APB_Alleycat, d=APB_Alleycata, y=2)

APB_Bivya <- SSfilter(x=ALB_prism, y="APB_Bivy")
APB_Bivy <- prism_unitsummary(d=ALB_prism, x="APB_Bivy")
APB_Bivy <- site_region_treat_ba(x=APB_Bivy, d=APB_Bivya, y=1)

APB_Chubb_Ea <- SSfilter(x=ALB_prism, y="APB_Chubb_E")
APB_Chubb_E <- prism_unitsummary(d=ALB_prism, x="APB_Chubb_E")
APB_Chubb_E <- site_region_treat_ba(x=APB_Chubb_E, d=APB_Chubb_Ea, y=3)

APB_Chubb_Wa <- SSfilter(x=ALB_prism, y="APB_Chubb_W")
APB_Chubb_W <- prism_unitsummary(d=ALB_prism, x="APB_Chubb_W")
APB_Chubb_W <- site_region_treat_ba(x=APB_Chubb_W, d=APB_Chubb_Wa, y=5)

APB_Dandya <- SSfilter(x=ALB_prism, y="APB_Dandy")
APB_Dandy <- prism_unitsummary(d=ALB_prism, x="APB_Dandy")
APB_Dandy <- site_region_treat_ba(x=APB_Dandy, d=APB_Dandya, y=3)

APB_Fowlersa <- SSfilter(x=ALB_prism, y="APB_Fowlers")
APB_Fowlers <- prism_unitsummary(d=ALB_prism, x="APB_Fowlers")
APB_Fowlers <- site_region_treat_ba(x=APB_Fowlers, d=APB_Fowlersa, y=3)

APB_Hippoa <- SSfilter(x=ALB_prism, y="APB_Hippo")
APB_Hippo <- prism_unitsummary(d=ALB_prism, x="APB_Hippo")
APB_Hippo <- site_region_treat_ba(x=APB_Hippo, d=APB_Hippoa, y=4)

APB_Hoffmana <- SSfilter(x=ALB_prism, y="APB_Hoffman")
APB_Hoffman <- prism_unitsummary(d=ALB_prism, x="APB_Hoffman")
APB_Hoffman <- site_region_treat_ba(x=APB_Hoffman, d=APB_Hoffmana, y=2)

APB_HuckMounda <- SSfilter(x=ALB_prism, y="APB_HuckMound")
APB_HuckMound <- prism_unitsummary(d=ALB_prism, x="APB_HuckMound")
APB_HuckMound <- site_region_treat_ba(x=APB_HuckMound, d=APB_HuckMounda, y=3)

APB_Humdingera <- SSfilter(x=ALB_prism, y="APB_Humdinger")
APB_Humdinger <- prism_unitsummary(d=ALB_prism, x="APB_Humdinger")
APB_Humdinger <- site_region_treat_ba(x=APB_Humdinger, d=APB_Humdingera, y=2)

APB_Hydroa <- SSfilter(x=ALB_prism, y="APB_Hydro")
APB_Hydro <- prism_unitsummary(d=ALB_prism, x="APB_Hydro")
APB_Hydro <- site_region_treat_ba(x=APB_Hydro, d=APB_Hydroa, y=5)

# combine all Albany data into one dataframe and export an excel -------------------


ALB_prism_sum_data <- rbind(APB_Hippo,
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


write_xlsx(ALB_prism_sum_data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/Summary_Data_Prism_ALB.xlsx")


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

ALB_prism_Control <- prism_treat_summary(d=ALB_prism_sum_data, x="Control", y=2)
ALB_prism_Control$Region <- "ALB"
ALB_prism_Control$Treat_Type <- "Control"
ALB_prism_Control <- prism_reorder(ALB_prism_Control)

ALB_prism_SpringRx <- prism_treat_summary(d=ALB_prism_sum_data, x="SpringRx", y=3)
ALB_prism_SpringRx$Region <- "ALB"
ALB_prism_SpringRx$Treat_Type <- "SpringRx"
ALB_prism_SpringRx <- prism_reorder(ALB_prism_SpringRx)

ALB_prism_Sp_MowRx <- ALB_prism_sum_data %>% 
  filter(Site %in% c("APB_Humdinger", "APB_Bivy", "APB_Alleycat")) %>% 
  group_by(Species_Groups) %>% 
  summarise(Avg_BA_eng = round((sum(BA_sf_a))/3, digits=2),
            Avg_BA_met =round((sum(BA_sm_ha))/3, digits=2)) %>% 
  dplyr::mutate(Mean_Site_BA_met = round(sum(Avg_BA_met), digits=0)) %>% 
  dplyr::mutate(Prop_of_Spec = (round((Avg_BA_met/Mean_Site_BA_met), digits=3))*100)
ALB_prism_Sp_MowRx$Region <- "ALB"
ALB_prism_Sp_MowRx$Treat_Type <- "MowRx"
ALB_prism_Sp_MowRx <- prism_reorder(ALB_prism_Sp_MowRx)

ALB_prism_Su_MowRx <- ALB_prism_sum_data %>% 
  filter(Site %in% c("APB_Chubb_W", "APB_Chubb_E", "APB_Dandy")) %>% 
  group_by(Species_Groups) %>% 
  summarise(Avg_BA_eng = round((sum(BA_sf_a))/3, digits=2),
            Avg_BA_met =round((sum(BA_sm_ha))/3, digits=2)) %>% 
  dplyr::mutate(Mean_Site_BA_met = round(sum(Avg_BA_met), digits=0)) %>% 
  dplyr::mutate(Prop_of_Spec = (round((Avg_BA_met/Mean_Site_BA_met), digits=3))*100)
ALB_prism_Su_MowRx$Region <- "ALB"
ALB_prism_Su_MowRx$Treat_Type <- "MowRx"
ALB_prism_Su_MowRx <- prism_reorder(ALB_prism_Su_MowRx)


# Now for MASS Data -------------------

CE_B7_Ba <- SSfilter(x=MA_prism, y="CE_B7_B")
CE_B7_B <- prism_unitsummary(d=MA_prism, x="CE_B7_B")
CE_B7_B <- site_region_treat_ba(x=CE_B7_B, d=CE_B7_Ba, y=4)

CE_B7_Ca <- SSfilter(x=MA_prism, y="CE_B7_C")
CE_B7_C <- prism_unitsummary(d=MA_prism, x="CE_B7_C")
CE_B7_C <- site_region_treat_ba(x=CE_B7_C, d=CE_B7_Ca, y=4)

CE_B7_Da <- SSfilter(x=MA_prism, y="CE_B7_D")
CE_B7_D <- prism_unitsummary(d=MA_prism, x="CE_B7_D")
CE_B7_D <- site_region_treat_ba(x=CE_B7_D, d=CE_B7_Da, y=3)

CE_BA3a <- SSfilter(x=MA_prism, y="CE_BA3")
CE_BA3 <- prism_unitsummary(d=MA_prism, x="CE_BA3")
CE_BA3 <- site_region_treat_ba(x=CE_BA3, d=CE_BA3a, y=3)

CE_BP20a <- SSfilter(x=MA_prism, y="CE_BP20")
CE_BP20 <- prism_unitsummary(d=MA_prism, x="CE_BP20")
CE_BP20 <- site_region_treat_ba(x=CE_BP20, d=CE_BP20a, y=3)

CE_SWB_E1a <- SSfilter(x=MA_prism, y="CE_SWB_E1")
CE_SWB_E1 <- prism_unitsummary(d=MA_prism, x="CE_SWB_E1")
CE_SWB_E1 <- site_region_treat_ba(x=CE_SWB_E1, d=CE_SWB_E1a, y=4)

CE_SWB_Na <- SSfilter(x=MA_prism, y="CE_SWB_N")
CE_SWB_N <- prism_unitsummary(d=MA_prism, x="CE_SWB_N")
CE_SWB_N <- site_region_treat_ba(x=CE_SWB_N, d=CE_SWB_Na, y=1)

CE_SWB_Sa <- SSfilter(x=MA_prism, y="CE_SWB_S")
CE_SWB_S <- prism_unitsummary(d=MA_prism, x="CE_SWB_S")
CE_SWB_S <- site_region_treat_ba(x=CE_SWB_S, d=CE_SWB_Sa, y=2)

CE_Wheelocka <- SSfilter(x=MA_prism, y="CE_Wheelock")
CE_Wheelock <- prism_unitsummary(d=MA_prism, x="CE_Wheelock")
CE_Wheelock <- site_region_treat_ba(x=CE_Wheelock, d=CE_Wheelocka, y=2)

Mashpee_B1a <- SSfilter(x=MA_prism, y="Mashpee_B1")
Mashpee_B1 <- prism_unitsummary(d=MA_prism, x="Mashpee_B1")
Mashpee_B1 <- site_region_treat_ba(x=Mashpee_B1, d=Mashpee_B1a, y=4)

Mashpee_Ca <- SSfilter(x=MA_prism, y="Mashpee_C")
Mashpee_C <- prism_unitsummary(d=MA_prism, x="Mashpee_C")
Mashpee_C <- site_region_treat_ba(x=Mashpee_C, d=Mashpee_Ca, y=3)

Mashpee_G3a <- SSfilter(x=MA_prism, y="Mashpee_C")
Mashpee_G3 <- prism_unitsummary(d=MA_prism, x="Mashpee_G3")
Mashpee_G3 <- site_region_treat_ba(x=Mashpee_G3, d=Mashpee_G3a, y=2)

MS_07aa <- SSfilter(x=MA_prism, y="MS_07a")
MS_07a <- prism_unitsummary(d=MA_prism, x="MS_07a")
MS_07a <- site_region_treat_ba(x=MS_07a, d=MS_07aa, y=4)

MS_09da <- SSfilter(x=MA_prism, y="MS_09d")
MS_09d <- prism_unitsummary(d=MA_prism, x="MS_09d")
MS_09d <- site_region_treat_ba(x=MS_09d, d=MS_09da, y=2)


# combine all MASS data into one dataframe and export an excel -------------------

MA_prism_sum_data <- rbind(CE_B7_B,
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

write_xlsx(MA_prism_sum_data, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/Summary_Data_Prism_MA.xlsx")

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

MA_prism_SpringRx <- prism_treat_summary(d=MA_prism_sum_data, x="SpringRx", y=3)
MA_prism_SpringRx$Region <- "MA"
MA_prism_SpringRx$Treat_Type <- "SpringRx"
MA_prism_SpringRx <- prism_reorder(MA_prism_SpringRx)

MA_prism_Control <- prism_treat_summary(d=MA_prism_sum_data, x="Control", y=2)
MA_prism_Control$Region <- "MA"
MA_prism_Control$Treat_Type <- "SpringRx"
MA_prism_Control <- prism_reorder(MA_prism_Control)

MA_prism_FallRx<- prism_treat_summary(d=MA_prism_sum_data, x="FallRx", y=3)
MA_prism_FallRx$Region <- "MA"
MA_prism_FallRx$Treat_Type <- "FallRx"
MA_prism_FallRx <- prism_reorder(MA_prism_FallRx)

MA_prism_Harvest<- prism_treat_summary(d=MA_prism_sum_data, x="Harvest", y=3)
MA_prism_Harvest$Region <- "MA"
MA_prism_Harvest$Treat_Type <- "Harvest"
MA_prism_Harvest <- prism_reorder(MA_prism_Harvest)

MA_prism_MowRx<- prism_treat_summary(d=MA_prism_sum_data, x="MowRx", y=3)
MA_prism_MowRx$Region <- "MA"
MA_prism_MowRx$Treat_Type <- "MowRx"
MA_prism_MowRx <- prism_reorder(MA_prism_MowRx)


# now binding them all together -------------------

# Control data -------------------
prism_control_data <- rbind(ALB_prism_Control,
                            LI_prism_Control,
                            NH_prism_Control,
                            MA_prism_Control,
                            ME_prism_Control)

prism_overall_control_data <- prism_overallsummary(x=prism_control_data, y=5)
prism_overall_control_data$Treat_Type <- "Control"


# MowRx data -------------------
prism_mowrx_data <- rbind(ALB_prism_Sp_MowRx,
                          ALB_prism_Su_MowRx,
                          MA_prism_MowRx,
                          NH_prism_MowRx)

prism_overall_mowrx_data <- prism_overallsummary(x=prism_mowrx_data, y=4)
prism_overall_mowrx_data$Treat_Type <- "MowRx"


# SpringRx data -------------------
prism_springrx_data <- rbind(ALB_prism_SpringRx,
                           LI_prism_SpringRx,
                           MA_prism_SpringRx)

prism_overall_springrx_data <- prism_overallsummary(x=prism_springrx_data, y=3)
prism_overall_springrx_data$Treat_Type <- "SpringRx"


# FallRx data -------------------
prism_fallrx_data <- rbind(MA_prism_FallRx,
                           ME_prism_FallRx,
                           NH_prism_FallRx)

prism_overall_fallrx_data <- prism_overallsummary(x=prism_fallrx_data, y=3)
prism_overall_fallrx_data$Treat_Type <- "FallRx"


# Harvest data -------------------
prism_harvest_data <- rbind(LI_prism_Harvest,
                            MA_prism_Harvest)

prism_overall_harvest_data <- prism_overallsummary(x=prism_harvest_data, y=2)
prism_overall_harvest_data$Treat_Type <- "Harvest"

# SPB data -------------------
prism_SPB_data <- LI_prism_SPB

prism_overall_SPB_data <- prism_overallsummary(x=prism_SPB_data, y=1)
prism_overall_SPB_data$Treat_Type <- "SPB"


# now one excel to rule them all -------------------

prism_summary <- rbind(prism_overall_control_data,
                       prism_overall_fallrx_data,
                       prism_overall_harvest_data,
                       prism_overall_mowrx_data,
                       prism_overall_SPB_data,
                       prism_overall_springrx_data)

write_xlsx(prism_summary, "/Users/user/Desktop/Data/Regen_RProj/DataObjects/PrismData/Overall_Summary_Data_Prism.xlsx")













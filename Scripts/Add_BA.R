# Stutzman
# Pitch Pine Regen Project: script to create Total BA (meters squared per HA) and PIRI BA to be attached to other dataframes
#  2023-11-29

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
P_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
P_Thinned_Data.csv5$Treat_Type <- "Harvest"


# merge into one data set -------------------
prism <- rbind(P_Control_Data.csv1,
               P_Fall_Burn_Data.csv2,
               P_Mow_Burn_Data.csv3,
               P_Spring_Burn_Data.csv4,
               P_Thinned_Data.csv5)

# remove merged datasets to keep envr clean -------------------
rm(P_Control_Data.csv1,
   P_Fall_Burn_Data.csv2,
   P_Mow_Burn_Data.csv3,
   P_Spring_Burn_Data.csv4,
   P_Thinned_Data.csv5)


# going to keep mutiple groups for now -------------------
# PIRI	
# TREE_OAKS	:: QUCO, QUAL, QUVE, QUPA, QURU, QUST
# PIST	
# ACRU	
# OTHER:: BEPO, NYSY, QUIL, SAAL, FAGR, QUPR, BENI, BELE, FRPE, MORSP, PRSP, ULAM, ACPE, POGR, PODE, POTR, PIRU, TSCA, PRSE

prism_live <- prism %>% 
  filter(Live_Total >= 1 | Latin_Name == 'No species found')

prism_live$Species_Groups <- NA

# PIRI
prism_live$Species_Groups <- ifelse(prism_live$Species_Code == 'PIRI', 'PIRI', prism_live$Species_Groups)

# TREE_OAKS	:: QUCO, QUAL, QUVE, QUPA, QURU, QUST
prism_live$Species_Groups <- ifelse(prism_live$Species_Code %in% c('QUCO', 'QUAL', 'QUVE', 'QUPA', 'QURU', 'QUST'), 'QUSP', prism_live$Species_Groups)

# PIST
prism_live$Species_Groups <- ifelse(prism_live$Species_Code == 'PIST', 'PIST', prism_live$Species_Groups)

#ACRU
prism_live$Species_Groups <- ifelse(prism_live$Species_Code == 'ACRU', 'ACRU', prism_live$Species_Groups)


# OTHER	:: NYSY, QUIL, SAAL, FAGR, QUPR, BENI, BELE, FRPE, MORSP, PRSP, ULAM
prism_live$Species_Groups <- ifelse(prism_live$Species_Code %in% c('NYSY', 'QUIL', 'SAAL', 'FAGR', 'QUPR', 'BENI', 'BELE', 'FRPE', 'MOSP', 'PRSP', 'ULAM', 'ACPE','PRSE','POGR', 'PODE', 'POTR', 'BEPO', 'PIRU', 'TSCA'), 'Other', prism_live$Species_Groups)

# None: NA
prism_live$Species_Groups <- ifelse(prism_live$Latin_Name == 'No species found', 'None', prism_live$Species_Groups)

check_ba <- prism_live %>% 
  filter(if_any(Species_Groups, is.na))

#filter for data I want
prism_live2 <- prism_live %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Live_Total)

# get the sites that had standing dead recorded only
prism_dead <- prism %>% 
  filter(Dead_Total >= 1, Live_Total == 0) %>% 
  select(Region, Treat_Type, Site, Plot_No, Live_Total)

prism_dead$Species_Groups <- 'None'

prism_dead2 <- prism_dead %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Live_Total)

prism_all_live <- rbind(prism_live2, prism_dead2)

n_distinct(prism_all_live$Plot_No)
#this yields 1001 plots

rm(prism_dead,
   prism_dead2)

# split data into meta and by site
prism_meta <- prism %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

prism3 <- prism_all_live %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Group_Total = (sum(Live_Total))*10) %>% 
  ungroup()

prism_complete <- prism3 %>% 
  complete(Plot_No, Species_Groups, fill = list(Group_Total = 0))

prism_NA <- prism_complete %>% 
  filter(Species_Groups %in% c('ACRU', 'Other', 'PIRI', 'PIST', 'QUSP'))

prism_merge <- merge(prism_meta, prism_NA, by = "Plot_No", )

rm(prism_live2,
   prism_meta,
   prism3,
   prism_complete)


# phew, now i've finally got the right number of plots ... now I need to make a data set with PIRI BA and total BA per plot

prism_BA.Total <- prism_merge %>% 
  group_by(Plot_No) %>% 
  summarise(Total_BA = (sum(Group_Total))) %>% 
  mutate(BA_HA = round(Total_BA * 0.22956))

prism_BA.Total$l.BA_HA <- log(prism_BA.Total$BA_HA + 1)


# rm(prism_NA, prism_BA.Total)


# now for just PIRI -------------------

prism_PIRI <- prism_merge %>% 
  filter(Species_Groups == "PIRI") %>% 
  mutate(PIRI.BA_HA = round((Group_Total) * 0.22956)) %>% 
  select(Plot_No, Group_Total, PIRI.BA_HA)

prism_PIRI$l.BA_piri <- log(prism_PIRI$PIRI.BA_HA + 1)

rm(check_ba,
   prism_all_live,
   prism_live,
   prism_merge,
   prism_NA)

just_PIRI <-  prism_PIRI %>% 
  select(Plot_No, l.BA_piri, PIRI.BA_HA)

#merge total BA and PIRI BA numbers into one dataset
prism_BA <- merge(prism_BA.Total, just_PIRI, by = "Plot_No")

#remove excess dataset to keep envr clean
rm(prism_PIRI,
   prism_BA.Total)


















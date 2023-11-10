# Stutzman
# Pitch pine regen project: prism/basal area code for logistic regression
#  2023-11-02

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)

# functions -------------------



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


# looking at live basal area -------------------
prism_live_sum <- prism %>% 
  group_by(Latin_Name) %>% 
  summarise(Total_Live_BA = sum(Live_Total))


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


# filtering prism live down to just the categories I want to retain -------------------
prism_live <- prism_live %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Live_Total)





#Unlike SS & LS, I don't need make the implied zeros visible in the basal area data, but we do want to know how many sites and which had a BA of 0, as this will be important for site average


# checking that all plots are in the dataset -------------------

prism_meta <- prism_live %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)


# must be 9 sites where there is only standing dead and no living BA that are getting dropped by the filter -------------------

prism_dead <- prism %>% 
  filter(Dead_Total >= 1, Live_Total == 0) %>% 
  select(Region, Treat_Type, Site, Plot_No)

z <- setdiff(prism_dead, prism_meta)
z$Species_Groups <- "None"
z$Live_Total <- 0


# so we found them and now we need to add them with info to the live dataframe, so it is no missing plots -------------------


prism_live2 <- rbind(prism_live, z)

n_distinct(prism_live2$Plot_No)
#this yields 1001 plots


# now to remove unused dataframes to keep things neat -------------------

rm(prism_dead,
   prism_live,
   prism_meta,
   z)


# now to check -------------------

check_prism <- prism_live2 %>% 
  filter(if_any(Species_Groups, is.na))

rm(check_prism)

# want to pivot wider so I can have ba for each species group on 1 line and the total site BA -------------------
# first I need to summarize duplicate columns post grouping -------------------


prism_live3 <- prism_live2 %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Group_Total = sum(Live_Total))

prism_meta <- prism_live2 %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

prism_live4 <- merge(prism_meta, prism_live3, by = 'Plot_No') %>% 
  group_by(Plot_No)



# transiting to wide format with one row for each plot -------------------
wide_prism <- prism_live4 %>% 
  select(Region, Treat_Type, Site, Plot_No, Species_Groups, Group_Total) %>% 
  pivot_wider(names_from = Species_Groups, values_from = Group_Total, values_fill = 0)

# dropping none category now 0 are represented by species and adding total  -------------------

wide_prism2 <- wide_prism %>% 
  select(Region, Treat_Type, Site, Plot_No, PIRI, QUSP, PIST, ACRU, Other) %>% 
  rowwise() %>% 
  mutate(Total_Plot_BA = sum(c(PIRI, QUSP, PIST, ACRU, Other)),
         T_BA_Acre = Total_Plot_BA*10,
         T_BA_HA = round(T_BA_Acre*0.404686, digits = 0))

# remove dataframes to keep things neat -------------------

rm(prism,
   prism_live2,
   prism_live3,
   prism_meta,
   prism_live_sum,
   wide_prism)


# 1 Acre  = 0.404686 HA


# graphing data to get a look at distro -------------------
# histogram of total BA frequency by treatment type
ggplot(wide_prism2, aes(x=Total_Plot_BA)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Treat_Type)

# I'm confused about how to have the total ba and show the proportion of each species?  -------------------

prism_live5 <- prism_live4 %>% 
  group_by(Plot_No) %>% 
  summarise(Plot_Total = sum(Group_Total))


prism_live6 <- merge(prism_live4, prism_live5, by = "Plot_No")


# now going to try and plot this to see if I can get a proportion? -------------------


ggplot(prism_live6, aes(x=Plot_Total, fill = Species_Groups)) +
  geom_bar(position = 'fill') +
  facet_grid(cols = vars(Treat_Type))

# not really sure that this informaion is helpful or really what I'm after at all.  -------------------

rm(prism_live4,
   prism_live5)








ggplot(prism_live4, aes(x=Group_Total, fill = Species_Groups)) +
  geom_bar(position = 'stack') +
  facet_grid(cols = vars(Treat_Type))

#histogram of overall BA frequency by treatment type
ggplot(prism_site_total, aes(x=T_BA_HA)) +
  geom_histogram(binwidth = 1) +
  facet_grid(rows = vars(Treat_Type))

#boxplot of BA by treat type
ggplot(prism_site_total, aes(x= Treat_Type, y= T_BA_HA))+
  geom_boxplot()

# boxplot of BA per treat type per region
ggplot(prism_site_total, aes(x= Region, y= T_BA_HA))+
  geom_boxplot()+
  facet_wrap(~Treat_Type)
# there is inconsistency in how treatments are applied across regions. Does region contain enough of this or should ownership be another random effect?

# I think I should scale BA up and then convert to per HA units










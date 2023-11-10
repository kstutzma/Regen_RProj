# Stutzman
# Pitch pine regen project : sapling code for logistic regression
#  2023-11-06

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)

# source functions -------------------


# global variables -------------------

file_folder_SA <- "CleanData/sapling_CSV/"

# Body -------------------

file_names_SA <- list.files(path=file_folder_SA)

#for loop to read in the files, copied from Bio381 class notes/homework 12

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

#Other  
sapling$Species_Groups <- ifelse(sapling$Species_Code %in% c('ACRU', 'BEPO', 'QUIL', 'QUPR', 'QUCO', 'QUAL', 'QUVE', 'QUST', 'QURU', 'QUPR', 'PRSE', 'FRPE', 'PRVI', 'PODE', 'POGR', 'SAAL', 'AMSP', 'FAGR', 'NYSY', 'ACPE', 'ACNE', 'MOSP', 'POTR', 'PRSP', 'PIST', 'ABBA', 'FRAL', 'ROPS', 'RHCA', 'ELUM', 'BELE'), 'Other', sapling$Species_Groups)


# now to check all have been assigned -------------------
 check_sa <- sapling %>% 
  filter(if_any(Species_Groups, is.na))

# there are 724 plots that do not have saplings recorded -------------------

rm(check_sa)

sapling <- sapling %>%
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups, 
         DBH.cm.)

# now what to do with it? Look into TPA data (T/HA) -------------------

# looks like I should have just tallied them and not recorded their DBH, maybe I'll condense into DBH categories? there really aren't a lot of saplings recorded, so that might break things up too much? -------------------

sapling$Tally <- NA

sapling$Tally <- ifelse(sapling$DBH.cm. >= 2.5, 1, sapling$Tally)

sapling$Tally <- ifelse(sapling$DBH.cm. == 0, 0, sapling$Tally)

# check to see all have been assigned -------------------
check_sapling <- sapling %>% 
  filter(if_any(Tally, is.na))

rm(check_sapling)


# add duplicates by plot and filter it down -------------------

sapling <- sapling %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Tally)

sapling$Species_Groups <- replace(sapling$Species_Groups, is.na(sapling$Species_Groups),'None')

# sapling2 <- sapling %>% 
#   group_by(Site) %>% 
#   summarise(No_of_plots = n_distinct(Plot_No),
#             Area_sampled = No_of_plots*0.0025)


n_distinct(sapling$Plot_No)
# 1001 plots, yay


sapling1 <- sapling %>% 
  group_by(Region,
           Treat_Type,
           Site,
           Plot_No, 
           Species_Groups) %>% 
  summarise(Spec_Total = sum(Tally))
            # Spec_per_HA = Spec_Total/0.0025)


ggplot(sapling1, aes(x=Spec_per_HA, fill = Species_Groups))+
  geom_histogram(position = 'stack')+
  facet_wrap(~Treat_Type)

ggplot(sapling1, aes(y=Spec_per_HA, x=Species_Groups))+
  geom_boxplot()+
  facet_wrap(~Treat_Type)


ggplot(sapling1, aes(y=Spec_per_HA, x=Treat_Type))+
  geom_boxplot()
# i'm getting confused about the zeros and still about showing the proportion, especially for BA -------------------



# adding a constant and then tranforming the data -------------------


sapling2 <- sapling1

sapling2$Constant_Total <- (sapling2$Spec_Total + 1)

sapling2$log_Total <- log(sapling2$Constant_Total)


# just look at PIRI -------------------

ss_PIRI <- ss_merge2 %>% 
  filter(Species_Groups == "PIRI")

# cleveland plots -------------------

dotchart(ss_PIRI$log_Total, 
         groups = factor(ss_PIRI$Treat_Type),
         pch=ss_PIRI$Treat_Type)


ss_pair <- ss_PIRI %>% 
  select(Region, Treat_Type, log_Total)

ggpairs(ss_pair)
#does not do what I want it to do













# make the two species groups two columns -------------------
# sapling1 <- sapling %>% 
#   group_by(Region,
#            Treat_Type,
#            Site,
#            Plot_No, 
#            Species_Groups) %>% 
#   summarise(Spec_Total = sum(Tally)) %>% 
#   pivot_wider(names_from = Species_Groups, values_from = Spec_Total, values_fill = NA)
# 
# # re-order data
# sapling1 <- sapling1 %>% 
#   select(Region,
#          Treat_Type,
#          Site,
#          Plot_No,
#          PIRI,
#          Other)
# 
# # get plot total and expand to per HA basis (plots are 1/400 HA)
# sapling3 <- sapling %>%
#   group_by(Plot_No) %>% 
#   summarise(Plot_Total = sum(Tally),
#             Total_Avg_HA = Plot_Total/0.0025)
# 
# 
# # merge two sapling dataframes together
# sa_merge1 <- merge(sapling1, sapling3, by = "Plot_No")
# 
# 
# #find the proportion of PIRI for saplings
# sa_merge2 <- sa_merge1 %>% 
#   group_by(Plot_No) %>% 
#   summarise(Prop_PIRI = round((PIRI/Plot_Total), digits = 2))
# 
# # merge this into sapling data
# sa_merge3 <- merge(sa_merge1, sa_merge2, by = 'Plot_No')
# 
# # re-order and keep columns wanted
# sa_merge3 <- sa_merge3 %>% 
#   select(Region,
#          Treat_Type,
#          Site,
#          Plot_No,
#          Plot_Total,
#          Total_Avg_HA,
#          Prop_PIRI)
# 
# # re-assign NAs as 0s
# sa_merge3$Prop_PIRI <- replace(sa_merge3$Prop_PIRI, is.na(sa_merge3$Prop_PIRI),0) 
# 
# 
# # remove dataframes to keep envr neat -------------------
# rm(check_ss,
#    sa_merge1,
#    sa_merge2,
#    sapling,
#    sapling1,
#    sapling2)
# 
# 
# # lets look at this in a histogram and boxplot -------------------
# 
# ggplot(sa_merge3, aes(x=Total_Avg_HA))+
#   geom_histogram()+
#   facet_wrap(~Treat_Type)
# 
# 
# ggplot(sa_merge3, aes(y=Total_Avg_HA, x=Treat_Type))+
#   geom_boxplot()


























# Stutzman
# Understory plant communities PPB: veg for NMDS
#  2024-02-07

# the plan is to run this a pure r code before cleaning it up and making it a markdown script to go on-line. 

# I will pivot data wider, fill implied zeros, reduce to species that occur in 5% of plots and then standardize columns

# libraries -------------------
library(ggplot2)
library(vegan)
library(ggvegan)
#library(ggpubr) #this masks mutate from plyr
library(tidyverse)
# library(plyr)
# library(dplyr)
# library(adespatial)

# source functions -------------------



# start by sourcing the veg script -------------------
source("Scripts/Veg_Data.R")
rm(veg3)


#split overall data from veg cover
veg_meta <- veg2 %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

#pivot veg data wider and fill in zeros
veg_wider <- veg2 %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0) 

#create dataframe of site and plot
veg_site <- veg2 %>% 
  select(Site, Plot_No) %>% 
  group_by(Site) %>% 
  distinct(Plot_No, .keep_all = TRUE)


merveg <- merge(veg_site, veg_wider, by = "Plot_No") %>% 
  select(-Plot_No)

#we've got 165 species (re-check post unidentified species list work)

# I'll need to go back and deal with unknowns, condense ID-ed grasses by genus etc, but for now we can play around a little







# source other files ----------------------

time_since <- read_csv("CleanData/Treat_Year_Data.csv") # this is a dataframe with one row for each site


# make sure ggpubs not on, or this won't load
source("Scripts/Add_BA.R") #work on prism_BA, has total BA figures - do I need to change these as they were collected on a different scale?

prism1 <- prism_BA %>% 
  select(Plot_No, BA_HA, PIRI.BA_HA)

prism2 <- merge(prism1, veg_site, by = "Plot_No") %>% 
  select(-Plot_No) %>% 
  select(Site, everything())

prism4veg <- prism2 %>% 
  group_by(Site) %>% 
  summarise_at(vars(BA_HA:PIRI.BA_HA), mean)

rm(prism_BA, prism1, prism2)


source("Scripts/Ground_Data.R") #work on ground 3

ground4 <- ground3 %>% 
  select(Plot_No, Lichen, Moss, Rock, Mineral_Soil, Wood, Litter_Duff, avgLD)

ground5 <- merge(ground4, veg_site, by = "Plot_No") %>% 
  select(Site, everything()) %>% 
  select(-c(Plot_No))

ground4veg <- ground5 %>% 
  group_by(Site) %>% 
  summarise_at(vars(Lichen:avgLD), mean)

rm(ground3, ground4, ground)


vegmeta2 <- veg_meta %>% 
  select(Region, Treat_Type, Site) %>% 
  distinct(Site, .keep_all = TRUE)

# now merge them (may have to think about relativization for BA numbers at some point - too hard currently)

vm1 <- merge(vegmeta2, time_since, by = "Site")

vm2 <- merge(vm1, ground4veg, by = "Site")

meta.data_veg <- merge(vm2, prism4veg, by = "Site") %>% 
  select(-c(Inventory_Yr, Treat_Yr))


rm(vm2, vm1, time_since, ground4veg, prism4veg)







#rule of thumb is less than 5% of plots. There are 499 plots. 5% of that is 25. that would leave me with 26 species

#THIS IS 20 plots or more, the last 5 are under 25 plots but above 20
veg_5PERC <- veg_wider %>% 
  select(Plot_No,
         QUIL,
         GAPR,
         VAPA,
         CAPE,
         GABA,
         VAAN,
         PTAQ,
         RUSP,
         KAAN,
         PIRI,
         ACRU,
         LYQU,
         QUCO,
         MELI,
         CEOR,
         COPE,
         PRSE,
         QUPR,
         PAQU,
         CEAM,
         QUAL,
         PIST,
         ARME,
         LYBO,
         POUN11,
         RHGL)
# AMSP,
# VAMY,
# FRSP,
# ARNU,
# SMGL)


z6 <- lapply(veg_5PERC, function(x){ length(which(x==0))})
z7 <- as_data_frame(z6)
(sum(z7[,2:27]))/(26*499)
#still 80% zeros with 26 species. feels ouch...
rm(z6, z7)




# add site and lose plot number -------------------
veg5 <- merge(veg_5PERC, veg_site) %>% 
  select(-Plot_No) %>% 
  select(Site, everything())

veg5_avg <- veg5 %>% 
  group_by(Site) %>% 
  summarise_at(vars(QUIL:RHGL), mean) %>% 
  column_to_rownames(var = "Site")

rm(veg5, veg_5PERC)

veg5_colsum <- as.data.frame(colSums(veg5_avg))
# CEOR is bittersweet and is only found in APB
# CEAM also an APB only, RHGL maybe too ... 

# relativize data, dividing each cell by the column total (to bring common and rare species closer together) -------------------
rel_veg5 <- decostand(veg5_avg, method = 'total', MARGIN = 2)

#umm ... am i ready to run?


# should look at Applied Community Ecology book, especially the decision tree stuff and start formally recording that business

# I have imported the data; pared the data down to species that occur in at least 5% of plots (aka 25 plots); averaged the data by site; relativized the data by column total. I will be using city-block distances (i.e., Bray-Curtis). Not going to transform the data further at this point

summary(rel_veg5)

#starting with 6 dimensions
test_6d <- metaMDS(rel_veg5, 
                   distance = 'bray', 
                   k = 6, 
                   trymax = 20,
                   autotransform = FALSE)
# this works and yields stress values of 0.06

# trying 5 dimensions
test_5d <- metaMDS(rel_veg5, 
                   distance = 'bray', 
                   k = 5, 
                   trymax = 20,
                   autotransform = FALSE)
# this works and yields stress values of 0.07 - 0.08

# trying 4 dimensions
test_4d <- metaMDS(rel_veg5, 
                   distance = 'bray', 
                   k = 4, 
                   trymax = 20,
                   autotransform = FALSE)
# this works and yields stress values of 0.09 - 0.1

# trying 3 dimensions
test_3d <- metaMDS(rel_veg5, 
                   distance = 'bray', 
                   k = 3, 
                   trymax = 20,
                   autotransform = FALSE)
# this works and yields stress values of 0.12

# trying 2 dimensions
test_2d <- metaMDS(rel_veg5, 
                   distance = 'bray', 
                   k = 2, 
                   trymax = 20,
                   autotransform = FALSE)
# this works and yields stress values of 0.16 (and one of 0.25)

# seems to me that 2 dimensions works well (below the 0.2 rule of thumb), but will keep 3 dimension as well

rm(test_4d, test_5d, test_6d)

# now I think I'll import all the other data, sum by site for quant data, to make a matrix of envr factors to compare against the NMDS





ordiplot(test_2d, type = "t")
# looks like region is the reason behind grouping ....

#trying out ggvegan

frt.nmds1 <- fortify(test_2d)





#plot 
fort<- fortify(test_2d)
ggplot() +
  geom_point(data = subset(fort, score =='sites'),
             mapping = aes(x = NMDS1, y = NMDS2),
             colour="black",
             alpha=0.5)+
  geom_segment(data = subset(fort, score == 'species'),
               mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type="closed"),
               colour="darkgray",
               linewidth =0.8)+
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept = 0,slope = 0,linetype="dashed", linewidth=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", linewidth=0.8, colour="gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))




ggplot() +
  geom_point(data = subset(fort, score =='sites'),
             mapping = aes(x = NMDS1, y = NMDS2, color = meta.data_veg$Treat_Type),
             alpha=0.5)+
  geom_segment(data = subset(fort, score == 'species'),
               mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type="closed"),
               colour="darkgray",
               linewidth =0.8)

#shape as TT, color as region
ggplot() +
  geom_text(data = subset(fort, score =='species'),
            mapping = aes(label=label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept = 0,slope = 0,linetype="dashed", linewidth=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", linewidth=0.8, colour="gray")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  geom_point(data = subset(fort, score =='sites'),
             mapping = aes(x = NMDS1, y = NMDS2, color = meta.data_veg$Region, shape = meta.data_veg$Treat_Type),
             alpha=2)

# region seems a pretty clear driver of understory composition. Can still run adonis tests etc on other values (permanova for treat or/and region)

# to run PerMANOVA, there is an assumption of evenness across envr factor, therefore check

summary(meta.data_veg)

summary(as.factor(meta.data_veg$Treat_Type))
# 6 (harvest) - 12 (mowrx): 6, 9, 9, 11, 12

summary(as.factor(meta.data_veg$Region))
# 5 (ME) - 14 (MA): 5, 8, 9, 11, 14


per.region <- adonis2(rel_veg5 ~ Region, data = meta.data_veg)

per.tt <- adonis2(rel_veg5 ~ Treat_Type, data = meta.data_veg)



























###################################
# figured out why veg had one more plot than ground (typo)
ground_plots <- ground3 %>% 
  select(Plot_No)

veg_plot <- veg_site %>% 
  ungroup() %>% 
  select(Plot_No)

setdiff(veg_plot, ground_plots)
#75 is different
























####################
#sort alphabetically column names (will need to pull plot to the front post)
v_sort <- veg_wider[,order(colnames(veg_wider))]

v_sort <- v_sort %>% 
  select(Plot_No, everything())



###### thinking about column & row sums for species
z <- colSums(veg_merge[,5:169])
z1 <- as.data.frame(z)

x <- rowSums(veg_merge[,5:169])
x1 <- as.data.frame(x)

rm(z, z1, x, x1)


########veg - 1 and - 2 species #############################################
# early and not so elegant code to understand how many of what species occur where
# I think I need to remove species columns that only occur once

veg_wider_log <- as.data.frame(lapply(veg_wider, as.logical))

z <- as.data.frame(colSums(veg_wider_log))
#this is a list of number of occurrences for each species. Will begin by removing all with only 1 occurrence 



veg_less1 <- select(veg_wider, -c("CHMA",
"CASp",
"ANQU",
"VACO",
"SMRA",
"SOPA",
"ARUV",
"ACPE",
"FAGR",
"ARHI",
"COTR",
"MEVI",
"CYAC",
"LYCL",
"DEDE",
"CHCA",
"MOCA",
"GOTE",
"NASP1",
"MOPE",
"ASSP11",
"RASP1",
"COUM",
"RHCA",
"CAUN12",
"CYLU",
"ASSP10",
"POUN13",
"HYSP1",
"SPAL",
"POTR",
"VETH",
"POUN9",
"PAVI",
"FOUN3",
"LYSP",
"ROSP",
"CAUN10",
"CAUN2",
"POUN3",
"POUN7",
"CISP",
"MOFI",
"SOUN4",
"POUN15",
"OSCL",
"OCAC",
"POUN19",
"POUN18",
"ACMI",
"POUN16",
"GAFR",
"HECA"))
  
  
#this worked. surely there is a more elegant way, but this worked. 
 
  
  
veg_less2 <- select(veg_less1, -c("ROMU",
                                 "ILVE",
                                 "MOUN",
                                 "IMSP",
                                 "OSCI",
                                 "VIAC",
                                 "SYFO",
                                 "QURU",
                                 "ILMU",
                                 "POGR",
                                 "CLAL",
                                 "BEPO",
                                 "PYAM",
                                 "SOCA",
                                 "SOSP",
                                 "VISP",
                                 "POUN1",
                                 "POUN12",
                                 "HISP1",
                                 "RHCO",
                                 "LECA",
                                 "UVPE",
                                 "FOUN2",
                                 "FOUN8",
                                 "BATI",
                                 "LIPH",
                                 "ERHI",
                                 "DEPU",
                                 "LYCI",
                                 "BOCY"))
  
# all of this will need to be re-evaluated after the unknown plant list has been revised/consolidated







#how many zeros in dataframe?
z2 <- lapply(veg_less1, function(x){ length(which(x==0))})
z3 <- as_data_frame(z2)
(sum(z3[,2:113]))/(113*499)
# is this is correct my data frame is 93.6% zeros... after removing those with one occurence


#trying after 2 occurances removed
z4 <- lapply(veg_less2, function(x){ length(which(x==0))})
z5 <- as_data_frame(z4)
(sum(z3[,2:83]))/(83*499)
#still 91.6% zeros 

rm(z2, z3, z4, z5)
##################################################
































# Stutzman
# Script to import veg, create site average, preform DS, reduce to species that occur in at least 5% of plots
#  2024-02-14

library(ggplot2)
library(vegan)
library(ggvegan)
library(tidyverse)
library(plyr)
library(dplyr)
library(adespatial)




# source functions -------------------
source("Functions/species_richness.R")
source("Functions/shannon_index.R")
source("Functions/spec_num.R")
source("Functions/Pielous_J.R")

#start by sourcing the veg script
source("Scripts/Veg_Data.R")
rm(veg3) # used by GLMMs and doesn't import right with dplyr activated, so just loose it so I stop worrying about it, checking, and then remembering ... 

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
  # dplyr:: mutate(No_of_Plots = n_distinct(Plot_No))

# veg_site2 <- veg_site %>%
#   select(Site, No_of_Plots) %>%
#   distinct(Site, .keep_all = TRUE)


# #merge site back into wide veg cover data frame
# site_veg <- merge(veg_site, veg_wider, by = "Plot_No")

#now to create a data frame where each site is averaged

#this groups by site and then finds the mean for each species at each site, then adds rowids to add site names back on at a later step
site_avg_veg <- site_veg %>%
  group_by(Site) %>% 
  summarise_at(vars(QUCO:HECA), mean)

# i would like to round this number, but I haven't figured out how to do it without losing the site column, so not worth it at the moment

#ok, I now have a mean for each species at each site. Next step is to preform DS for each site - actually that should be done prior to averaging ...

  
  
  
  
  
#need to put site back into veg wider frame to try filtering and piping sac by site


veg_merge1 <- merge(veg_site, veg_wider, by = "Plot_No")

veg_merge2 <- veg_merge1 %>% 
  select(Site, Plot_No, everything())

#what information do I want site by site?

# observed species richness (jackknife estimates) - specpool(df, smallsample = T)
# per point species richness - "specnumber(dataframe)" pull out mean, median, sd 
# shannon index (per point diversity) - "diversity(df, index="shannon"), pull out mean and SD
# species mean, sd, median, & number of points
# sorenson dissamilarity index
  



# i would like to double check all this to make sure i've got the calculations and formulas correct and to add sorenson.

# then i'd like to write a function that does this and pulls out the relevant stats

# then think about a for loop that does it for each site (or lapply?)



















############################### lots of useless(?) code - replaced by understanding partioning

#starting by converting data set to presence/absence

#need to fix names
#veg_merge2 <- veg_merge2 %>% 
  select(-Site)

vm_rn <- data.frame(veg_merge2[,-1], row.names=veg_merge2[,1])

vm_rn[vm_rn > 0] <- 1

vm_rn <- rownames_to_column(vm_rn, var = "Plot_No")

veg_pa <- merge(veg_site, vm_rn, by = "Plot_No")
#holy shit. i did that

spc_sumbysite <- veg_pa %>% 
  group_by(Site) %>% 
  summarise_at(vars(QUCO:HECA), sum)

#RICHNESS
richness <- ddply(spc_sumbysite, ~Site, function(x) {
  data.frame(Richness=sum(x[-1]>0))
}) # we got it. AMEN baby. this was terrible.

#ABUNDANCE - not sure how useful as this is % cover and not individuals counted, but ok
abun1 <- veg_merge1 %>%
  group_by(Site) %>% 
  summarise_at(vars(QUCO:HECA), mean)

abundance <- ddply(abun1, ~Site, function(x) {
  data.frame(Abundance = sum(x[-1]))
})

#################################################

















#starting work on veg_merge2

veg_merge3 <- veg_merge2 %>% 
  select(-Plot_No)

split_veg <- split(veg_merge3[-1], veg_merge3$Site)



# Species Richness (jackknife and chao etc estimates) -------------------
sp <- as.data.frame(sapply(split_veg, species_richness))

#transpose
spcrich_bysite <- as.data.frame(t(sp))
#AMAZING!!!!!!
rm(sp)



# Shannon index -------------------
si <- as.data.frame(sapply(split_veg, shannon_index))

#transpose
si_bysite <- as.data.frame(t(si))

#name columns
colnames(si_bysite) <- c("S.I._Mean", "S.I._SD")

rm(si)



# Number of species -------------------
spc_n <- as.data.frame(sapply(split_veg, spec_num))

#transpose
spcnum_bysite <- as.data.frame(t(spc_n))

#name columns
colnames(spcnum_bysite) <- c("Spc.Num_Mean", "Spc.Num_SD")

rm(spc_n)





# Evenness ------------------- should be SI diversity / log of species number - this function does omit sites that are NaN. These sites only have 1 species recorded. The log of 1 is 0 and anything divided by 0 is infinite (this is a total of 3 sites, one in each: APB_Hippo, RPD2_B2, & RPD2_B7)

even1 <- as.data.frame(sapply(split_veg, Pielous_J))

even2 <- as.data.frame(t(even1))

colnames(even2) <- c("Pielous_J_mean", "Pielous_J_SD")

rm(even1)

# test issues with APB_Hippo, RPD2_B2, & RPD2_B7
HIPPO <-  veg_merge3 %>% 
  filter(Site == "RPD2_B7") %>% 
  select(-Site)

H <- diversity(HIPPO)
S <- specnumber(HIPPO)
J <- H/(log(S))

print(J)

rm(H, S, J, HIPPO)






# now to think about Sorensen dissimilarity

# creating a dataframe where observations from every subplot within a site are added together to use for P/A calculations in sorensen
veg4soren <- veg_merge3 %>% 
  group_by(Site) %>% 
  summarise_at(vars(QUCO:HECA), sum) %>% 
  column_to_rownames(var = "Site")

site_rowid <- veg_merge3 %>% 
  group_by(Site) %>% 
  summarise_at(vars(QUCO:HECA), sum) %>% 
  rowid_to_column(var="rowid") %>% 
  select(Site, rowid)



BDiv <- beta.div.comp(veg4soren, coef = "S", quant = FALSE)
#overall values
BetaD.df <- as.data.frame(BDiv$part)
#distance matrix for all sites
BetaD.distances <- as.matrix(BDiv$D)
#replacement values matrix for all sites
BetaD.repl.matrix <- as.matrix(BDiv$repl)
#richness values matrix for all sites
BetaD.rich.matrix <- as.matrix(BDiv$rich)




#looking at replacement and richness values within beta diversity calculations
replacement <- LCBD.comp(BDiv$repl, sqrt.D = TRUE)

rep.df <- as.data.frame(replacement$LCBD)

names(rep.df)[names(rep.df) == "replacement$LCBD"] <- "replace.lcbd"

rep.df <-  rep.df %>% 
  rowid_to_column(var="rowid")

rep.df2 <- merge(rep.df, site_rowid, by = "rowid") %>%
  select(Site, replace.lcbd) %>% 
  column_to_rownames(var = "Site")


#richness
richness <- LCBD.comp(BDiv$rich, sqrt.D = TRUE)

rich.df <- as.data.frame(richness$LCBD)

names(rich.df)[names(rich.df) == "richness$LCBD"] <- "richness.lcbd"

rich.df <- rich.df %>% 
  rowid_to_column(var = "rowid")

rich.df2 <- merge(rich.df, site_rowid, by = "rowid") %>% 
  select(Site, richness.lcbd) %>% 
  column_to_rownames(var = "Site")

rm(rep.df, rich.df)











# merging data sets of DS
m1 <- merge(spcrich_bysite, si_bysite, by="row.names") %>% 
  column_to_rownames(var = "Row.names")


m2 <- merge(m1, spcnum_bysite, by="row.names") %>% 
  column_to_rownames(var = "Row.names")

m3 <- merge(m2, even2, by = "row.names") %>% 
  column_to_rownames(var = "Row.names")


m4 <- merge(m3, rich.df2, by = "row.names") %>% 
  column_to_rownames(var = "Row.names")

veg_sd <- merge(m4, rep.df2, by = "row.names") %>% 
  column_to_rownames(var = "Row.names")


rm(m1, 
   m2, 
   m3, 
   m4)









 # KEEPING CODE, BUT BOY DID I WASTE SOME TIME  
#holy shit - did i not need to do any of this because i was thinking of sum and not mean - idiot.   
# rowid_to_column(var="rowid")
# # merge with number of plots per site
# try2 <- merge(veg_site2, try1, by = "Site")
# #pull out row id number and site in order to be able to reattach site after dplyr numeric transformation
# site_rowid <- try2 %>% 
#   select(Site, rowid)
# #divide mean by number of plots
# site_avg <- round((try2[,4:168]/try2[,2]), digits = 3) %>% 
#   rowid_to_column(var="rowid")
# site_avgF <- merge(site_rowid, site_avg, by = "rowid")
















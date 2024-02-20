# Stutzman
# Understory plant communities PPB: veg for NMDS
#  2024-02-07

# the plan is to run this a pure r code before cleaning it up and making it a markdown script to go on-line. 

# would like to pivot wider and see how that works for the many species zeros...

#then generate some descriptive stats on species abundance, diversity, etc - using both treatment type and region

library(ggplot2)
library(vegan)
library(devtools)
library(ggvegan)
library(tidyverse)


#start by sourcing the veg script
source("Scripts/Veg_Data.R")

#work off veg2

#divide into two dataframes to merge back together later? I don't think they merge back together - as I need two matrix to compare for NMDS

#if i want to look at TT and Region for DS, I may need to sort those first here and then run DS on each individual

veg_meta <- veg2 %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

veg_wider <- veg2 %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0) 

#yes! this worked


veg_merge <- merge(veg_meta, veg_wider, by = "Plot_No")

#we've got 165 species (re-check post unidentified species list work)

# I'll need to go back and deal with unknowns, condense ID-ed grasses by genus etc, but for now we can play around a little


SAC <- specaccum(comm = veg_wider, method = "random", permutations = 1000)

plot(SAC)
boxplot(SAC) #looks weird. Would like to be able to plot by region or treat type, which she had in that video...

SPC_num <- specnumber(veg_wider)
mean(SPC_num) #7.3
sd(SPC_num) #2.5
median(SPC_num) #7
fivenum(SPC_num) #min 2, 1Q 5, median 7, 3Q 9, max 17

S.I <- diversity(veg_wider, index = "shannon")

plot(S.I)
mean(S.I)   # 0.47
sd(S.I)     # 0.22

x <- specpool(veg_wider, smallsample = T)
# i want to look at by treat type and this is all pooled. seems like i might need to filter and create individual and then overlay them. Seems difficult, but I am interested as to whether these vary by treatment type and region


#sort alphabetically column names (will need to pull plot to the front post)
v_sort <- veg_wider[,order(colnames(veg_wider))]

v_sort <- v_sort %>% 
  select(Plot_No, everything())

#how many zeros in dataframe?
z <- lapply(v_sort, function(x){ length(which(x==0))})

z1 <- as_data_frame(z)

(sum(z1[,2:166]))/(165*499)
# is this is correct my data frame is 96% zeros...
#wonder how this changes when I remove species with only one occurrence ... 

rm(z, z1)

n_distinct(v_sort$Plot_No) #says there are 499 distinct plots - I'm pretty sure there are only 498 in regen, which is weird and I should investigate


# I think looking at all of this by treatment type is interesting, so I'm going to create these subgroups, as introducing the factor in the vegan code just doesn't seem possible

c.veg <- veg2 %>% 
  filter(Treat_Type == "Control") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

mrx.veg <- veg2 %>% 
  filter(Treat_Type == 'MowRx') %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

srx.veg <- veg2 %>% 
  filter(Treat_Type == "SpringRx") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

frx.veg <- veg2 %>% 
  filter(Treat_Type == "FallRx") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

h.veg <- veg2 %>% 
  filter(Treat_Type == "Harvest") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

# Species accumulation curves by treatment type
sac.c <- specaccum(comm = c.veg, method = "random", permutations = 1000)

sac.mrx <- specaccum(comm = mrx.veg, method = "random", permutations = 1000)

sac.srx <- specaccum(comm = srx.veg, method = "random", permutations = 1000)

sac.frx <- specaccum(comm = frx.veg, method = "random", permutations = 1000)

sac.h <- specaccum(comm = h.veg, method = "random", permutations = 1000)

sac.t <- specaccum(comm = veg_wider, method = "random", permutations = 1000)

#Here are the species accumulation curves by each treatment type, layered into one graph. Mowing has the most point and species
plot(sac.t) #total
lines(sac.mrx, lty=1, lwd=1, col="yellow") #mowrx
lines(sac.srx,lty=1,lwd=1,col="green") #springrx
lines(sac.c,lty=1,lwd=1,col="blue") #control
lines(sac.frx,lty=1,lwd=1,col="red") #fallrx
lines(sac.h,lty=1,lwd=1,col="purple") #harvest

# rm(sac.srx,
#    sac.frx,
#    sac.c,
#    sac.h,
#    sac.mrx)

#species richness
SPC_c <- specnumber(c.veg)
mean(SPC_c)
sd(SPC_c) 
median(SPC_c) 

SPC_h <- specnumber(h.veg)
mean(SPC_h)
sd(SPC_h) 
median(SPC_h) 

SPC_mrx <- specnumber(mrx.veg)
mean(SPC_mrx)
sd(SPC_mrx) 
median(SPC_mrx) 

SPC_srx <- specnumber(srx.veg)
mean(SPC_srx)
sd(SPC_srx) 
median(SPC_srx) 

SPC_frx <- specnumber(frx.veg)
mean(SPC_frx)
sd(SPC_frx) 
median(SPC_frx) 

SPC_t <- specnumber(veg_wider)
mean(SPC_t)
sd(SPC_t) 
median(SPC_t)

#diversity index
si.c <- diversity(c.veg, index = "shannon")
mean(si.c)
sd(si.c)

si.h <- diversity(h.veg, index = "shannon")
mean(si.h)
sd(si.h)

si.mrx <- diversity(mrx.veg, index = "shannon")
mean(si.mrx)
sd(si.mrx)

si.srx <- diversity(srx.veg, index = "shannon")
mean(si.srx)
sd(si.srx)

si.frx <- diversity(frx.veg, index = "shannon")
mean(si.frx)
sd(si.frx)

si.t <- diversity(veg_wider, index = "shannon")
mean(si.t)
sd(si.t)

plot(si.t)
points(si.mrx, col = "yellow")
points(si.h, col="purple")
points(si.c, col="blue")
points(si.srx, col="green")
points(si.frx, col="red")

boxplot(si.t, si.h, si.srx, si.mrx, si.frx, si.c)

#diversity estimator, using chao
sp.c <- specpool(c.veg, smallsample = T)
sp.h <- specpool(h.veg, smallsample = T)
sp.mrx <- specpool(mrx.veg, smallsample = T)
sp.srx <- specpool(srx.veg, smallsample = T)
sp.frx <- specpool(frx.veg, smallsample = T)
sp.t <- specpool(veg_wider, smallsample = T)












#I'd also like to see by region
LI.veg <- veg2 %>% 
  filter(Region == "LI") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

ALB.veg <- veg2 %>% 
  filter(Region == "ALB") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

MA.veg <- veg2 %>% 
  filter(Region == "MA") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

NH.veg <- veg2 %>% 
  filter(Region == "NH") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

ME.veg <- veg2 %>% 
  filter(Region == "ME") %>% 
  select(Plot_No, Species_Code, SP_Cover_midpoint) %>% 
  group_by(Plot_No) %>% 
  pivot_wider(names_from = Species_Code, values_from = SP_Cover_midpoint, values_fill = 0)

# Species accumulation curves by region
sac.LI <- specaccum(comm = LI.veg, method = "random", permutations = 1000)

sac.ALB <- specaccum(comm = ALB.veg, method = "random", permutations = 1000)

sac.MA <- specaccum(comm = MA.veg, method = "random", permutations = 1000)

sac.NH <- specaccum(comm = NH.veg, method = "random", permutations = 1000)

sac.ME <- specaccum(comm = ME.veg, method = "random", permutations = 1000)

#Here are the species accumulation curves by each region, layered into one graph. Albany has the most species, MA has the most points
plot(sac.ALB) #ALB
lines(sac.MA,lty=1,lwd=1,col="green") #MA
lines(sac.LI,lty=1,lwd=1,col="blue") #LI
lines(sac.NH,lty=1,lwd=1,col="red") #NH
lines(sac.ME,lty=1,lwd=1,col="purple") #ME

# rm(sac.ALB,
#    sac.LI,
#    sac.MA,
#    sac.ME,
#    sac.NH)



###### thinking about column & row sums for species
z <- colSums(veg_merge[,5:169])
z1 <- as.data.frame(z)

x <- rowSums(veg_merge[,5:169])
x1 <- as.data.frame(x)

rm(z, z1, x, x1)



# richness, diversity, abundance and evenness for each plot
# i'm doing this by plot, but wondering about doing it by site? need to revist PC-ORD text about the proportion stuff

#richness
library(plyr)
richness <- ddply(veg_wider,~Plot_No,function(x) {
  data.frame(RICHNESS=sum(x[-1]>0))
  })

ggplot(richness, aes(RICHNESS))+
  geom_histogram()
# not normal, but unimodal - transform?


#abundance
abundance <- ddply(veg_wider,~Plot_No,function(x) {
  data.frame(ABUNDANCE=sum(x[-1]))
  })


#diversity
diversity <-  ddply(veg_wider,~Plot_No,function(x) {
  data.frame(SHANNON=diversity(x[-1], index="shannon"))
  })

#evenness
even <- ddply(veg_wider,~Plot_No,function(x) {
  data.frame(SIMPSON=diversity(x[-1], index="simpson")/log(sum(x[-1]>0)))
  })


a <- merge(richness, abundance, by = "Plot_No")

b <- merge(a, diversity, by = "Plot_No")

div.met <- merge(b, even, by = "Plot_No")

rm(a, b, diversity, even, abundance, richness)

div.met$EVENNESS <- div.met$SHANNON/div.met$RICHNESS



ggplot(div.met, aes(RICHNESS))+
  geom_histogram()
# not normal, but unimodal, positive skew

ggplot(div.met, aes(ABUNDANCE))+
  geom_histogram()
# not normal, but unimodal, positive skew

ggplot(div.met, aes(SHANNON))+
  geom_histogram()
# not normal, but unimodal, negative skew

ggplot(div.met, aes(SIMPSON))+
  geom_histogram()
# not normal, but unimodal, long tails both sides

ggplot(div.met, aes(EVENNESS))+
  geom_histogram()
# not normal, but unimodal, negative skew?









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































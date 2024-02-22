# Stutzman
# PP regen project: understory veg analysis. Script for spec accum curves, overall DS
#  2024-02-21

# libraries -------------------
library(ggplot2)
library(vegan)
library(ggvegan)
library(tidyverse)

# source functions -------------------
source("Functions/curve_spc.R")



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

#merge datasets
df <- merge(veg_meta, veg_wider, by = "Plot_No")


# Species accumulation curves
SAC <- specaccum(comm = veg_wider, method = "random", permutations = 1000)
plot(SAC)

# overall species number
SPC_num <- specnumber(veg_wider)
mean(SPC_num) #7.3
sd(SPC_num) #2.5
median(SPC_num) #7
fivenum(SPC_num) #min 2, 1Q 5, median 7, 3Q 9, max 17

# overal diversity
S.I <- diversity(veg_wider, index = "shannon")

plot(S.I)
mean(S.I)   # 0.47
sd(S.I) 

# over all richness estimates
x <- specpool(veg_wider, smallsample = T)
# i want to look at by treat type and this is all pooled. seems like i might need to filter and create individual and then overlay them. Seems difficult, but I am interested as to whether these vary by treatment type and region


# how many zeros in data frame?
z <- lapply(veg_wider[-1], function(x){ length(which(x==0))})

z1 <- as_tibble(z)

(sum(z1))/(165*499) #df is 96% zeros
rm(z, z1)

n_distinct(veg_wider$Plot_No) #499 plots


df2 <- df %>% 
  select(-c(Plot_No, Region, Site))

trt_split <- split(df2[-1], df2$Treat_Type)

sp.ac <- sapply(trt_split, curve_spc)

sac.t <- specaccum(comm = veg_wider, method = "random", permutations = 1000)

plot(sac.t)
#plotting from the split code hasn't worked. I'll include the code below from before I knew about split and did everything manually. Just to keep it.

# unclear if I'll actually use this, so won't come back to it unless I do.




















#################################################
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

#####################################################

#Also for region
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









#####################################################
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
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


# global variables -------------------

file_folder_SS <- "CleanData/smallseedling_CSV/"

# Body -------------------

file_names_SS <- list.files(path=file_folder_SS)

#for loop to read in the files, copied from Bio381 class notes/homework 12

for (i in seq_along(file_names_SS)) {
  assign(paste0(file_names_SS[i], i),
         read.csv(paste0(file_folder_SS, file_names_SS[i])))
  }

# Adding treatment types ------------------- not loading in SPB or harvest data, due to few regional replicates

SS_Control_Data.csv1$Treat_Type <- "Control"
SS_Fall_Burn_Data.csv2$Treat_Type <- "FallRx"
SS_Mow_Burn_Data.csv3$Treat_Type <- "MowRx"
SS_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
SS_Thinned_Data.csv5$Treat_Type <- "Harvest"


# merge into one data set -------------------

small_seedling <- rbind(SS_Control_Data.csv1, 
                        SS_Fall_Burn_Data.csv2, 
                        SS_Mow_Burn_Data.csv3, 
                        SS_Spring_Burn_Data.csv4,
                        SS_Thinned_Data.csv5) 

# remove merged data sets to keep envr clean -------------------
rm(SS_Control_Data.csv1, 
   SS_Fall_Burn_Data.csv2, 
   SS_Mow_Burn_Data.csv3, 
   SS_Spring_Burn_Data.csv4,
   SS_Thinned_Data.csv5)



# Changing species grouping -------------------
# ACRU	
# PIRI	
# Shrub Oaks ::	QUIL, QUPR
# Tree Oak ::	QUCO, QUAL, QUVE, QURU
# HARDWOOD ::	AMPS, BELE, BEPO, BESP, PODE, POGR, POTR, MASP, NYSY, ACPE, CASP, CRSP, FAGR, FRAM, FRAL, ROPS, RHCA, PRSE, PRVI, PRPE, PRSP
# SOFTWOOD ::	PIST, JUCO

# create a new empty column and then re-assign groups ------------------- 

small_seedling$Species_Groups <- NA

# ACRU
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code == 'ACRU', 'ACRU', small_seedling$Species_Groups)

# PIRI	
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code == 'PIRI', 'PIRI', small_seedling$Species_Groups)


# Shrub Oaks ::	QUIL, QUPR
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code %in% c('QUIL', 'QUPR'), 'Shrub_Oak', small_seedling$Species_Groups)


# Tree Oak ::	QUCO, QUAL, QUVE, QURU
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code %in% c('QUCO', 'QUVE', 'QUAL', 'QURU'), 'Tree_Oak', small_seedling$Species_Groups)


# HARDWOOD ::	AMSP, BELE, BEPO, BESP, PODE, POGR, POTR, MASP, NYSY, ACPE, CASP, CRSP, FAGR, FRAM, FRAL, ROPS, RHCA, SAAL, SAHU
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code %in% c('AMSP', 'BELE', 'BEPO', 'BESP', 'PODE', 'POGR', 'POTR', 'MASP', 'NYSY', 'ACPE', 'CASP', 'CRSP', 'FAGR', 'FRAM', 'FRAL', 'ROPS', 'RHCA', 'SAAL', 'SAHU', 'PRSE','PRVI','PRPE','PRSP'), 'Other_Hardwood', small_seedling$Species_Groups)

# SOFTWOOD ::	PIST, JUCO
small_seedling$Species_Groups <- ifelse(small_seedling$Species_Code %in% c('PIST', 'JUCO'), 'Other_Softwood', small_seedling$Species_Groups)

# now to check -------------------
check_SS <- small_seedling %>% 
  filter(if_any(Species_Groups, is.na))

# select for just the information I want, can lose species code and latin name -------------------
small_seedling <- small_seedling %>% 
  select(Region, 
         Treat_Type,
         Site, 
         Plot_No,
         Species_Groups, 
         Total, 
         Browsed,
         StumpSprout,
         Germinate)


# I am going to try running some basic summary statistics here -------------------

SS_sumstats <- small_seedling %>% 
  pivot_longer(c(Total, Browsed, StumpSprout, Germinate), names_to = "Type") %>% 
  group_by(Region, Treat_Type, Site, Species_Groups, Type) %>% 
  summarize(average = round(mean(value), digits=1),
            med = median(value),
            standard_deviation = round(sd(value), digits = 1),
            variance = round(var(value), digits = 1),
            min = min(value),
            max = max(value),
            .groups = "drop")


# lose the NAs -------------------
SS_sumstatsNA <- SS_sumstats[complete.cases(SS_sumstats),]
# I wonder if this will be the same for mean when I add in the zeros? -------------------

# not sure about outliers - they certainly exist, with so many plots with 0, plots with high counts (max 59 at one plot) create outliers









    
# I need to split the treat type, site, region data off or else it adds way too much info when i use 'complete' below -------------------

small_seedling_meta <- small_seedling %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)


small_seedling_plot <- small_seedling %>% 
  select(Plot_No, Species_Groups, Total, Browsed, StumpSprout, Germinate)


# duplicates of "other_hardwood' etc need to be combined into one row before using complete -------------------
small_seedling_plot1 <- small_seedling_plot %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Total = sum(Total),
            Browsed = sum(Browsed),
            StumpSprout = sum(StumpSprout),
            Germinate = sum(Germinate)) %>% 
  ungroup()


small_seedling_plot2 <- small_seedling_plot1 %>% 
  complete(Plot_No, Species_Groups, fill = list(Total = 0, Browsed = 0, StumpSprout = 0, Germinate = 0))

# so that worked the way I wanted it to, I check a coupe of random plots against the original data. It does now carry an NA row for each point, which I will get rid of now -------------------

small_seedlingNA <- small_seedling_plot2[complete.cases(small_seedling_plot2),] 
  
# now to join it back with the meta data -------------------

ss_merge <- merge(small_seedling_meta, small_seedlingNA, by = "Plot_No")


# remove dataframes no longer needed -------------------
rm(small_seedling,
   small_seedling_meta,
   small_seedling_plot,
   small_seedling_plot1,
   small_seedling_plot2,
   small_seedlingNA,
   check_SS)



# yes. this is right -------------------


ss_merge2 <- ss_merge

ss_merge2$Site <- as.factor(ss_merge2$Site)
ss_merge2$Species_Groups <- as.factor(ss_merge2$Species_Groups)
ss_merge2$Treat_Type <- as.factor(ss_merge2$Treat_Type)

hist(ss_merge2$Total)

ggplot(ss_merge2, aes(x=Total)) +
  geom_histogram() +
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))


# find mean for each site and create a histogram of that information -------------------

ss_merge3 <- ss_merge2 %>% 
  group_by(Treat_Type, Site, Species_Groups) %>% 
  summarise(Site_avg = mean(Total),
            Site_sd = sd(Total))


ggplot(ss_merge3, aes(x=Site_avg)) +
  geom_histogram(binwidth = 5) +
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))

















# for each unit, lets think about finding the total mean, proportion of germs, ss, and browsed, along with min, max, sd, var, and maybe IQR? -------------------


SpRx_SS_unit_sumall <- SpRx_SS %>% 
  group_by(Treat_Type, Site, Species_Groups) %>% 
  summarise(T_mean = round(mean(Total), digits = 4),
            T_browse = round(mean(Browsed), digits = 4),
            Prcnt_Browse = round((T_browse)/(sum(Total)), digits = 3),
            T_ss = round(mean(StumpSprout), digits = 4),
            Prcnt_ss = round((T_ss)/(sum(Total)), digits = 3),
            T_germ = round(mean(Germinate), digits = 4),
            Prct_ss = round((T_germ)/(sum(Total)), digits = 3))



SpRx_SS_unit_sum <- SpRx_SS %>% 
  group_by(Treat_Type, Site, Species_Groups) %>% 
  summarise(T_mean = round(mean(Total), digits = 4),
            med = median(Total),
            standard_deviation = round(sd(Total), digits = 1),
            variance = round(var(Total), digits = 1),
            min = min(Total),
            max = max(Total))
  
# maybe these should all be independent data frames? I need to know more about what get passed into the ANOVA -------------------


# pivot_wider(names_from = Species_Groups, values_from = c(T_mean, Prcnt_Browse))









# I need to do QA/QC, but doing it on the original data set (w/o grouping, zeros added) seems like a crazy amount of work, so I'm going to start doing it here -------------------

ME_sumdat <- ME_SS %>% 
  pivot_longer(c(Total, Browsed, StumpSprout, Germinate), names_to = "Type") %>% 
  group_by(Region, Treat_Type, Site, Species_Groups, Type) %>% 
  summarize(average = round(mean(value), digits=3),
            minimum = min(value),
            q25 = quantile(value, probs = 0.25),
            med = median(value),
            q75 = quantile(value, probs = 0.75),
            maximum = max(value),
            standard_deviation = sd(value),
            .groups = "drop")

# this is so much information to look at -------------------

# maybe some box plots to see if anything is off? -------------------





















































  
  
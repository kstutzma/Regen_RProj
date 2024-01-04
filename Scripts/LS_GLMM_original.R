# Stutzman
# Pitch pine regen project : large seedling code for logistic regression
#  2023-11-01

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
# library(moments)
# library(lme4)
# library(lmerTest)
# library(kableExtra)
# library(mosaic)
# library(car)
# library(TMB)
# library(multcomp)
library(emmeans)
# library(writexl)
# library(openxlsx)
# library(doBy)
library(DHARMa)

# source functions -------------------

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
LS_Spring_Burn_Data.csv4$Treat_Type <- "SpringRx"
LS_Thinned_Data.csv5$Treat_Type <- "Harvest"

# merge into one data set -------------------

large_seedling <- rbind(LS_Control_Data.csv1, 
                        LS_Fall_Burn_Data.csv2, 
                        LS_Mow_Burn_Data.csv3,
                        LS_Spring_Burn_Data.csv4, 
                        LS_Thinned_Data.csv5)

# remove merged datasets to keep the envr clean -------------------

rm(LS_Control_Data.csv1, 
   LS_Fall_Burn_Data.csv2, 
   LS_Mow_Burn_Data.csv3, 
   LS_Spring_Burn_Data.csv4, 
   LS_Thinned_Data.csv5)



# change species groupings -------------------
# PIRI
# Other

large_seedling$Species_Groups <- NA

# PIRI
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code == 'PIRI', 'PIRI', large_seedling$Species_Groups)

# Shrub Oak
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('QUPR', 'QUIL'), 'Shrub Oak', large_seedling$Species_Groups)

# Other
large_seedling$Species_Groups <- ifelse(large_seedling$Species_Code %in% c('ACRU', 'QUCO', 'QUAL', 'QUVE', 'QURU', 'QUPA', 'SAAL', 'AMSP', 'BELE', 'BEPO', 'BENI', 'PODE', 'POGR', 'POTR', 'ACPE', 'FRAM', 'SAHU', 'FAGR', 'MASP', 'ACNE', 'CRSP', 'FRAL', 'ROPS', 'RHCA', 'ELUM', 'FRPE', 'PRSE', 'PRVI', 'PRPE', 'PIST', 'ABBA'), 'Other', large_seedling$Species_Groups)

# now to check that all species have been assigned and only NAs are NAs in new column -------------------

check_ls <- large_seedling %>% 
  filter(if_any(Species_Groups, is.na))


# select for just the information I want, can lose species code and latin name -------------------

large_seedling <- large_seedling %>% 
  select(Region,
         Treat_Type,
         Site,
         Plot_No,
         Species_Groups,
         Total,
         Browsed,
         StumpSprout)

#split treat type, site, region data off -------------------
large_seedling_meta <- large_seedling %>% 
  select(Region, Treat_Type, Site, Plot_No) %>% 
  distinct(Plot_No, .keep_all = TRUE)

large_seedling_plot <- large_seedling %>% 
  select(Plot_No, Species_Groups, Total, Browsed, StumpSprout)

# duplicates of "other" need to be combined into one row before using complete -------------------
large_seedling_plot1 <- large_seedling_plot %>% 
  group_by(Plot_No, Species_Groups) %>% 
  summarise(Total = sum(Total),
            Browsed = sum(Browsed),
            StumpSprout = sum(StumpSprout)) %>% 
  ungroup()

large_seedling_plot2 <- large_seedling_plot1 %>% 
  complete(Plot_No, Species_Groups, fill = list(Total = 0, Browsed = 0, StumpSprout = 0))

# get rid of NAs -------------------
large_seedlingNA <- large_seedling_plot2[complete.cases(large_seedling_plot2),]

# now to join it back with the meta data -------------------
ls_merge <- merge(large_seedling_meta, large_seedlingNA, by = 'Plot_No')


n_distinct(ls_merge$Plot_No)
# this yields 1001  plots

# remove dataframes no longer needed -------------------
rm(check_ls,
   large_seedling,
   large_seedling_meta,
   large_seedling_plot,
   large_seedling_plot1,
   large_seedling_plot2,
   large_seedlingNA)







# ********  exploring total data using histogram -------------------
ggplot(ls_merge, aes(x=Total))+
  geom_histogram(binwidth = 2)+
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))


# ********  boxplots? -------------------

ggplot(ls_merge, aes(x = Species_Groups, y=Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))

# ********  lets look at some summary statistics -------------------

ls_sumstats2 <- ls_merge %>% 
  pivot_longer(c(Total, Browsed, StumpSprout), names_to = "Type") %>% 
  group_by(Treat_Type, Species_Groups, Type) %>% 
  summarize(average = round(mean(value), digits=1),
            med = median(value),
            sd = round(sd(value), digits = 1),
            var = round(var(value), digits = 1),
            min = min(value),
            max = max(value),
            .groups = "drop")


by(ls_merge, ls_merge$Treat_Type, summary)






# filter just for PIRI

ls_piri <- ls_merge %>% 
  filter(Species_Groups == "PIRI") %>% 
  arrange(Treat_Type)

# make explanatory variables factors -------------------
ls_piri$Treat_Type = factor(ls_piri$Treat_Type,
                              levels = unique(ls_piri$Treat_Type))

ls_piri$Region = factor(ls_piri$Region,
                          levels = unique(ls_piri$Region))

ls_piri$Site = factor(ls_piri$Site,
                        levels = unique(ls_piri$Site))




# THIS CODE IS FROM BEFORE I PUT ADDITIONAL DATA (LIKE GROUND COVER AND BA) INTO THE DATA SET #####################

ls.nb_null <- glmer.nb(Total~Treat_Type + (1|Site/Plot_No),
                       data = ls_piri)
summary(ls.nb_null) # AIC = 493, df resid = 993


ls.zi_null2 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                      data = ls_piri,
                      ziformula = ~1,
                      family = nbinom1)
#other one has convergence problems


AIC(ls.zi_null2) # null = 493, zi2 = 497

rm(ls.zi_null2)

# for now, looks like just NB model is better than zero-inflated. now add more fixed and random effects?

ls.nb_region <- glmer.nb(Total~Treat_Type + (1|Site/Plot_No) + (1|Region),
                       data = ls_piri)
summary(ls.nb_region) # AIC = 496

rm(ls.nb_region)


ls.nb_sim.resid <- simulateResiduals(ls.nb_null, n = 1000, plot = TRUE) #looks pretty good

testDispersion(ls.nb_sim.resid)
testOutliers(ls.nb_sim.resid)

plotResiduals(ls.nb_sim.resid, ls_piri$Treat_Type, xlab = "Treatment Type", main = NULL)

plotResiduals(ls.nb_sim.resid, ls_piri$Site, xlab = "Site", main = NULL)

# plotResiduals(ls.nb_sim.resid, ls_merge3$Plot_No, xlab = "Plot Number", main = NULL) # not sure if i should be making this plot or exactly what it means, but it does show significant deviation in the 3rd quantile  

testZeroInflation(ls.nb_sim.resid) # looking pretty good

# ok, the model fits look pretty good all things considered, now I need to understand how to run post-hoc pairwise comparisons -------------------


emmeans(ls.nb_null, list(pairwise~Treat_Type), adjust = "tukey", type = "response") #not significant







# GOING TO ADD TIME SINCE, BA, AND GROUND COVER DATA TO THE DATAFRAME

time_since <- read_csv("CleanData/Treat_Year_Data.csv")

ls_piri <- merge(ls_piri, time_since, by = 'Site')
ls_piri$l.TFT <- log(ls_piri$Time_from_Treat)


# ****** RUN R SCRIPT 'Add_BA.R' to generate the basal area data to add to the large seedling dataframes - dataframe called prism_BA
ls.merge1 <- merge(ls_piri, prism_BA, by = "Plot_No")


# now to run the Ground_Data script and add that to the data frame -

ls_alldata <- merge(ls.merge1, ground3, by = "Plot_No")


rm(ls.merge1,
   ls_piri)






##################################################
# now starting to run some models with more parts -------------------

ls.p_all <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                  data = ls_alldata,
                  family = poisson)
summary(ls.p_all) #failed to converge, AIC is 291

ls.p2 <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + (1|Site/Plot_No) + offset(l.TFT),
                  data = ls_alldata,
                  family = poisson)
summary(ls.p2) #failed to converge, AIC is 289.2

ls.p3 <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + + (1|Site/Plot_No) + offset(l.TFT),
               data = ls_alldata,
               family = poisson)
summary(ls.p3) #failed to converge, AIC is 287.1

ls.p4 <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + + (1|Site/Plot_No),
               data = ls_alldata,
               family = poisson)
summary(ls.p4) #failed to converge, AIC is 286.8


ls.p5 <- glmer(Total~Treat_Type + avgLD_l + (1|Site/Plot_No),
               data = ls_alldata,
               family = poisson)
summary(ls.p5) #AIC is 284.4

ls.p6 <- glmer(Total~Treat_Type + (1|Site/Plot_No),
               data = ls_alldata,
               family = poisson)
summary(ls.p6) #AIC is 287.5


#best model so far --------------------
ls.p7 <- glmer(Total~Treat_Type + avgLD_l+ (1|Site/Plot_No) + offset(l.TFT),
               data = ls_alldata,
               family = poisson)
summary(ls.p7) #AIC is 284.6



library(lmtest)
lrtest(ls.p7, ls.p5) #with offset p is less than 0.0001, so keep it

ls.p8 <- glmer(Total~Treat_Type + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
               data = ls_alldata,
               family = poisson)
summary(ls.p8) #288


rm(ls.p2,
   ls.p3,
   ls.p4,
   ls.p5,
   ls.p6,
   ls.p8)



ls.p7 <- glmer(Total~Treat_Type + avgLD_l+ (1|Site/Plot_No) + offset(l.TFT),
               data = ls_alldata,
               family = poisson)
summary(ls.p7) #AIC is 284.6
#This is the best model



#  look at DHARMa

ls.p7_sr <- simulateResiduals(fittedModel = ls.p7, n = 1000, plot = TRUE)



testDispersion(ls.p7_sr) #PASSES
testOutliers(ls.p7_sr)
testZeroInflation(ls.p7_sr) #looks good

# Plotting standardized residuals against predictors
plotResiduals(ls.p7_sr, ls_alldata$Treat_Type, xlab = "Treatment Type", main=NULL)  

plotResiduals(ls.p7_sr, ls_alldata$Site, xlab = "Site", main=NULL)



# run emmeans for pairwise

emmeans(ls.p7, specs = pairwise ~ Treat_Type, adjust = 'Tukey', type = 'response')
# harvest is the only significantly different



library(lmtest)

rm(ls.p_all,
   ls.p10,
   ls.p8)



library(sjPlot)
library(sjlabelled)
library(sjmisc)

set_theme(base = theme_classic(),
          theme.font = 'serif',
          axis.title.size = 1.5,
          axis.textsize.x = 1.5,
          axis.textsize.y = 1.5,
          title.size = 2.5,
          title.align = "center",
          legend.pos = "right",
          legend.size = 1.5,
          legend.title.size = 1.5,
          #legend.bordercol = "black",
          legend.item.size = .75)


# LS plot 1 -------------------


plot_model(ls.p9, 
           type = 'pred', 
           terms = c('l.avgLD', 'Treat_Type'),
           axis.title = c("Average Leaf Litter Depth (log transformed)", "Total Count of Pitch Pine"),
           title = "Predicted Counts of Pitch Pine Seedlings \n >/=50cm & <2.5cm DBH",
           legend.title = "Treatment Type",
           line.size = 1,
           value.offset = 'Treat_Type',
           ci.lvl = NA,
           colors = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15")) 




























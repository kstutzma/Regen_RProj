# Stutzman
# Pitch pine regen project : sapling code for logistic regression
#  2023-11-06

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
#library(GGally)
# # library(moments)
library(lme4)
library(lmerTest)
# library(kableExtra)
# library(mosaic)
# library(car)
# library(writexl)
# library(openxlsx)
# library(AER)
library(glmmTMB)
library(TMB)
# library(multcomp)
library(emmeans)
library(DHARMa)

# source functions -------------------

source("Scripts/SA_Import.R")



# filter for just piri -------------------

sapling_PIRI <- sapling_merge %>% 
  filter(Species_Groups == "PIRI") %>% 
  arrange(Treat_Type)
# PIRI saplings are only in 50 of the 1001 plots and mostly in control sites....


# now to make the explanatory variables factors

sapling_PIRI$Treat_Type = factor(sapling_PIRI$Treat_Type,
                              levels = unique(sapling_PIRI$Treat_Type))

sapling_PIRI$Region = factor(sapling_PIRI$Region,
                          levels = unique(sapling_PIRI$Region))

sapling_PIRI$Site = factor(sapling_PIRI$Site,
                        levels = unique(sapling_PIRI$Site))




# GOING TO ADD TIME SINCE, BA, AND GROUND COVER DATA TO THE DATAFRAME
time_since <- read_csv("CleanData/Treat_Year_Data.csv")

sapling_PIRI2 <- merge(sapling_PIRI, time_since, by = 'Site')
sapling_PIRI2$l.TFT <- log(sapling_PIRI2$Time_from_Treat)



# ****** RUN R SCRIPT 'Add_BA.R' to generate the basal area data to add to the large seedling dataframes - dataframe called prism_BA
sa.merge1 <- merge(sapling_PIRI2, prism_BA, by = 'Plot_No')



# now to run the Gound_Data script and add that to the data frame -
sa_alldata <- merge(sa.merge1, ground3, by = 'Plot_No')


rm(#sa.merge1, keeping this data set to be able to run models that operate on all the observations, as LD not important
   sapling_PIRI2)




##################################################
# now starting to run some models with more parts ------------------- previously this was zero inflated, so I'm going to start with a negative binomial distribution


sa.nb_all <- glmer.nb(Total_Tally~Treat_Type + l.BA_HA + l.BA_piri + l.Mineral + avgLD_l + (1|Site/Plot_No) + offset(l.TFT),
                      data = sa_alldata)
summary(sa.nb_all) #failed to converge


sa.nb2 <- glmer.nb(Total_Tally~Treat_Type + l.Mineral + avgLD_l + (1|Site/Plot_No) + offset(l.TFT),
                      data = sa_alldata)
summary(sa.nb2) #failed to converge, AIC is 217


sa.p3 <- glmer(Total_Tally~Treat_Type + avgLD_l + (1|Site/Plot_No) + offset(l.TFT),
               data = sa_alldata,
               family = poisson)
summary(sa.p3) #first model that didn't fail to converge, AIC is 213.6


######## this is the best model
sa.p4 <- glmer(Total_Tally~Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
               data = sa_alldata,
               family = poisson)
summary(sa.p4) #AIC is 211.7

# would like to try this model again, but with all the data
sa.p5 <- glmer(Total_Tally~Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
               data = sa.merge1,
               family = poisson)
summary(sa.p5) #it fails to converge with all data




sa.p6 <- glmer(Total_Tally~Treat_Type + (1|Site/Plot_No),
               data = sa_alldata,
               family = poisson)
summary(sa.p6) #AIC is 211.3
library(lmtest)
lrtest(sa.p4, sa.p6) #same p-value as the offset in other tests, wonder what that's about

sa.p7 <- glmer(Total_Tally~Treat_Type + l.BA_piri + (1|Site/Plot_No) + offset(l.TFT),
               data = sa_alldata,
               family = poisson)
summary(sa.p7)
lrtest(sa.p4, sa.p7) #p = .2, not worth the l.BA_piri

rm(sa.p6, 
   sa.p5, 
   sa.p7,
   sa.p3, 
   sa.nb2, 
   sa.nb_all)


#  look at DHARMa --------------------------------

sa.p4_sr <- simulateResiduals(fittedModel = sa.p4, n=1000, plot = TRUE)

testDispersion(sa.p4_sr)
testZeroInflation(sa.p4_sr)





######## this is the best model
sa.zi4 <- glmmTMB(Total_Tally~Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
               data = sa_alldata,
               ziformula=~1,
               family = poisson)
summary(sa.zi4) #AIC is 257.7 - this passes the DHARMa test

# would like to try this model again, but with all the data
sa.zi9 <- glmmTMB(Total_Tally~Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
                  data = sa.merge1,
                  ziformula=~1,
                  family = poisson)
summary(sa.zi9) #AIC is 411.1 - lets check with dharma ... would it be easier to keep this clean in an r markdown file?

sa.zi9_sr <- simulateResiduals(fittedModel = sa.zi9, n = 1000, plot = TRUE) #significant differences

testDispersion(sa.zi9_sr)# does not pass
testOutliers(sa.zi9_sr)
testZeroInflation(sa.zi9_sr) #does not pass
#looks like it is better to use half the data, which is what i'm using for the other two measurements, so at least it is consistent





sa.zi5 <- glmmTMB(Total_Tally~Treat_Type + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                  data = sa_alldata,
                  ziformula=~1,
                  family = poisson)
summary(sa.zi5) #AIC is 215.4, SR don't agree - not better!


sa.zi6 <- glmmTMB(Total_Tally~Treat_Type + l.BA_piri + (1|Site/Plot_No) + offset(l.TFT),
                  data = sa_alldata,
                  ziformula=~1,
                  family = poisson)
summary(sa.zi6) #AIC is 214.2, SR don't agree - not better!

sa.zi7 <- glmmTMB(Total_Tally~Treat_Type + l.BA_piri + (1|Site/Plot_No),
                  data = sa_alldata,
                  ziformula=~1,
                  family = poisson)
summary(sa.zi7) #AIC is 213.9, SR don't agree - not better!


lrtest(sa.zi6, sa.zi7) #same p value for offset 


sa.zi8 <- glmmTMB(Total_Tally~Treat_Type + (1|Site/Plot_No),
                  data = sa_alldata,
                  ziformula=~1,
                  family = poisson)
summary(sa.zi8) #doesn't pass the DHARMa test





sa.zi4_sr <- simulateResiduals(fittedModel = sa.zi4, n = 1000, plot = TRUE)

testDispersion(sa.zi4_sr)# PASSES
testOutliers(sa.zi4_sr)
testZeroInflation(sa.zi4_sr) #looking a lot better



sa.zi8_sr <- simulateResiduals(fittedModel = sa.zi8, n = 1000, plot = TRUE) 

plotResiduals(sa.zi4_sr, sa_alldata$Treat_Type, xlab = "Treatment Type", main=NULL)  

plotResiduals(sa.zi4_sr, sa_alldata$Site, xlab = "Site", main=NULL)



# run emmeans for pairwise

emmeans(sa.zi4, specs = pairwise ~ Treat_Type, adjust = 'Tukey', type = 'response')





rm(sa.p4,
   sa.p4_sr,
   sa.zi5,
   sa.zi5_sr,
   sa.zi6,
   sa.zi6_sr,
   sa.zi7,
   sa.zi7_sr,
   sa.zi8,
   sa.zi8_sr)




# time to move on to plotting!!! -------------------


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



plot_model(sa.zi4, 
           type = 'pred', 
           terms = c('Treat_Type', 'l.TFT'),
           axis.title = c("Average Leaf Litter Depth (log transformed)", "Total Count of Pitch Pine"),
           title = "Predicted Counts of Pitch Pine Seedlings <50cm",
           legend.title = "Treatment Type",
           line.size = 1,
           value.offset = 'Treat_Type',
           ci.lvl = NA,
           colors = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15")) 



plot_model(sa.zi4, 
           type = "pred",
           terms = "Treat_Type")



plot_model(sa.zi4, 
           type = "pred",
           terms = c("l.TFT", "Treat_Type"),
           axis.title = c("Time from Last Treatment (log transformed)", "Total Count of Pitch Pine"),
           title = "Predicted Counts of Pitch Pine Saplings \n >/= 2.5 cm & < 10 cm DBH",
           legend.title = "Treatment Type",
           line.size = 1,
           show.zeroinf = T,
           ci.lvl = NA,
           colors = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"))















##################### this is my original analysis of all plot before I added more data / variables to the model -------------------
sa.nb_null <- glmer.nb(Total_Tally~Treat_Type + (1|Site/Plot_No),
                       data = sapling_PIRI)

summary(sa.nb_null) #AIC is 434

sa.zi_null <- glmmTMB(Total_Tally~Treat_Type + (1|Site/Plot_No),
                       ziformula=~1,
                       family = nbinom2,
                       data=sapling_PIRI)

summary(sa.zi_null) #AIC is higher - need to learn more about how to fit/check the fit of models



# using DHARMa to check the model fit -------------------

sa_sim.resid <- simulateResiduals(sa.nb_null, n = 1000, plot = TRUE) #model differs significatly

testDispersion(sa_sim.resid)
testOutliers(sa_sim.resid)

plotResiduals(sa_sim.resid, sapling_PIRI$Treat_Type, xlab = "Treatment_Type", main=NULL)

plotResiduals(sa_sim.resid, sapling_PIRI$Site, xlab = "Site", main=NULL)

outliers(sa_sim.resid) #integer(0) is the given response???

testZeroInflation(sa_sim.resid) #this data appears to be zero inflated, so I have to figure out more about writing zero inflated code and then recheck


sa.zi_region <- glmmTMB(Total_Tally~Treat_Type + (1|Site/Plot_No),
                      zi=~Region,
                      family = nbinom2,
                      data=sapling_PIRI)
summary(sa.zi_region) #AIC is 467.7, more than null model, but null model is a poor fit
#nbinom1 as family has model convergence problems

sa.zi_sim.resid <- simulateResiduals(sa.zi_region, n=1000, plot = TRUE)

testZeroInflation(sa.zi_sim.resid) #holy shit, this looks good


# plot residuals against predictors/random variables/zi assignment
plotResiduals(sa.zi_sim.resid, sapling_PIRI$Treat_Type, xlab = "Treat_Type", main = NULL)

plotResiduals(sa.zi_sim.resid, sapling_PIRI$Site, xlab = "Site", main = NULL)

plotResiduals(sa.zi_sim.resid, sapling_PIRI$Region, xlab = "Region", main = NULL)

# post-hoc pairwise comparison -------------------
emmeans(sa.zi_region, pairwise~Treat_Type, adjust = "Tukey", type = "response") #not significant














































































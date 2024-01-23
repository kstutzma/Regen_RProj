# Stutzman
# Pitch pine regen project: small seedling code for logistic regression
#  2023-11-01

# libraries -------------------
library(dplyr)
library(tidyverse)
library(ggplot2)
# library(moments)
library(lme4)
library(lmerTest)
#library(kableExtra)
#library(mosaic)
#library(car)
# library(writexl)
# library(openxlsx)
# library(AER)
library(glmmTMB)
library(TMB)
# library(multcomp)
library(emmeans)
library(DHARMa)
# library(gap)
#library(multcompView)

# functions -------------------

# global variables -------------------


# Body -------------------

# start by sourcing the file that imports and prepares for analysis all small seedling data
source("Scripts/SS_Import.R")





# ******** graphing data to get a look at distro -------------------
ggplot(ss_merge, aes(x=Total)) +
  geom_histogram(binwidth = 1) +
  facet_grid(cols = vars(Species_Groups), rows = vars(Treat_Type))

# ********  boxplots? -------------------

ggplot(ss_merge, aes(x = Species_Groups, y=Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))



# ********  lets look at some summary statistics -------------------
ss_sumstats2 <- ss_merge %>% 
  pivot_longer(c(Total, Browsed, StumpSprout, Germinate), names_to = "Type") %>% 
  group_by(Treat_Type, Species_Groups, Type) %>% 
  summarize(average = round(mean(value), digits=1),
            med = median(value),
            sd = round(sd(value), digits = 1),
            var = round(var(value), digits = 1),
            min = min(value),
            max = max(value),
            .groups = "drop")

rm(ss_sumstats2)
 


# filter for just PIRI
ss_PIRI <- ss_merge %>% 
  filter(Species_Groups == "PIRI") %>% 
  arrange(Treat_Type)

# now to make the explanatory variables factors

ss_PIRI$Treat_Type = factor(ss_PIRI$Treat_Type,
                              levels = unique(ss_PIRI$Treat_Type))

ss_PIRI$Region = factor(ss_PIRI$Region,
                              levels = unique(ss_PIRI$Region))

ss_PIRI$Site = factor(ss_PIRI$Site,
                              levels = unique(ss_PIRI$Site))


# ********  look at descriptive stats -------------------

kable(favstats(Total ~ Site, data = ss_merge2),
      booktabs = T, format = ,
      caption = "PIRI counts by Site") %>% 
  kable_styling("striped", full_width = T)

kable(favstats(Total ~ Treat_Type, data = ss_merge2),
      booktabs = T, format = ,
      caption = "PIRI counts by Site") %>% 
  kable_styling("striped", full_width = T)

kable(favstats(log_Total ~ Treat_Type, data = ss_merge2),
      booktabs = T, format = ,
      caption = "PIRI counts by Site") %>% 
  kable_styling("striped", full_width = T)

kable(favstats(log_Total ~ Site, data = ss_merge2),
      booktabs = T, format = ,
      caption = "PIRI counts by Site") %>% 
  kable_styling("striped", full_width = T)




############### original analysis looking at nb and zi distros #####################
ss.nb_null <- glmer.nb(Total~Treat_Type + (1|Site/Plot_No),
                     data = ss_PIRI)
summary(ss.nb_null) #AIC : 1388 df. resid : 993

ss.zi_null <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                      ziformula=~1,
                      family = nbinom2,
                      data=ss_PIRI)

ss.zi_null2 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                      ziformula=~1,
                      family = nbinom1,
                      data=ss_PIRI)

AIC(ss.nb_null, ss.zi_null, ss.zi_null2) #Best AIC with nb_null model at 1388 (1398 for zi_null, and 1402 for zi_null2)

rm(ss.zi_null, 
   ss.zi_null2)


# for now, looks like just NB model is better than zero-inflated. now add more fixed and random effects?

ss.nb_region <- glmer.nb(Total~Treat_Type + (1|Site/Plot_No) + (1|Region),
                         data = ss_PIRI)
summary(ss.nb_region) #AIC 1390

ss.nb_plot2 <- glmer.nb(Total~Treat_Type + (1|Plot_No) + (1|Region),
                        data = ss_PIRI) 
summary(ss.nb_plot3) #AIC 1401

AIC(ss.nb_null, ss.nb_region) # with region, has a larger AIC (1397), also some warning message about "boundary (singular) fit: see help('isSingular')"

# i should check the fit of the model? whatever is akin to residuals?



#how to plot fixed effects against residuals?

# post hoc comparison
par(mfrow = c(1,1))

with(ss_PIRI, interaction.plot(Region, Treat_Type, Total))

emmeans(ss.nb_null, pairwise ~ Treat_Type)


rm(ss.nb_plot2,
   ss.nb_region)



# using DHARMa package to check model fit -------------------

ss.nb_sim.resid <- simulateResiduals(fittedModel = ss.nb_null, n = 1000, plot = TRUE)

testDispersion(ss.nb_sim.resid)
testOutliers(ss.nb_sim.resid)

# Plotting standardized residuals against predictors
plotResiduals(ss.nb_sim.resid, ss_merge2$Treat_Type, xlab = "Treatment Type", main=NULL)  
  
plotResiduals(ss.nb_sim.resid, ss_merge2$Site, xlab = "Site", main=NULL)  #two are significantly different, but can't say which

outliers(ss.nb_sim.resid) #gives 658, no idea what that means

testZeroInflation(ss.nb_sim.resid) #p value of 0.6, looks pretty normal/possible and therefore not zero inflated
  








# now would like to import time since data set and join to ss data set -------------------

time_since <- read_csv("CleanData/Treat_Year_Data.csv")

ss_merge1 <- merge(ss_PIRI, time_since, by = "Site")
ss_merge1$l.TFT <- log(ss_merge1$Time_from_Treat)






# ****** RUN R SCRIPT 'Add_BA.R' to generate the basal area data to add to the small seedling dataframes - dataframe named prism-BA





# merge with ss dataset -------------------
ss_merge2 <- merge(ss_merge1, prism_BA, by = "Plot_No")


# now to run the Ground_Data script and add that to the data frame ------------------- data set is called ground3, litter depth measurements are still in wide format, haven't figured that out



ss_alldata <- merge(ss_merge2, ground3, by = "Plot_No")


# remove some datasets that were merged to create final, boss data set -------------------
rm(ss_merge2,
   ss_merge1,
   ss_PIRI,
   time_since)









# start with biggest model ------------------- this is only on half of the observations (those with veg and soil data)

ss.p_all <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                  data = ss_alldata,
                  family = poisson)
summary(ss.p_all) #issues with convergence AIC is 732.4


ss.p2 <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + (1|Site/Plot_No),
                  data = ss_alldata,
                  family = poisson)
summary(ss.p2) #AIC is 715.2, issue with convergence

############## no longer best model, due to mistake double transforming LD - now fails to converge
ss.p3 <- glmer(Total~Treat_Type + avgLD_l + l.BA_HA + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson)
summary(ss.p3) # AIC 711.5, issues with converge

rm(ss.p2, ss.p_all)


ss.p4 <- glmer(Total~Treat_Type + l.Mineral + (1|Site/Plot_No),
           data = ss_alldata,
           family = poisson)
summary(ss.p4) #AIC is 717.7, converges


#now this is the best model **********************
ss.p5 <- glmer(Total~Treat_Type + avgLD_l + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson)
summary(ss.p5) # converged, AIC 714.6



ss.p6 <- glmer(Total~Treat_Type + avgLD_l + l.BA_piri + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson)
summary(ss.p6) #failed to converge


ss.p7 <- glmer(Total~Treat_Type + l.BA_piri + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson)
summary(ss.p7) #AIC is 719


ss.p8 <- glmer(Total~Treat_Type + l.BA_piri + l.Mineral + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson)
summary(ss.p8) # failed to coverge

ss.p9 <- glmer(Total~Treat_Type + avgLD_l + l.Mineral + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson) #failed to converge


rm(ss.p4,
  ss.p7, ss.p8, ss.p6, ss.p9)





############## this is the best model
ss.p5 <- glmer(Total~Treat_Type + avgLD_l + (1|Site/Plot_No),
               data = ss_alldata,
               family = poisson)
summary(ss.p5) # converged, AIC 714.6

# now running DHARMa and checking model #######

ss.p5_sr <- simulateResiduals(fittedModel = ss.p5, n= 1000, plot = TRUE)

testDispersion(ss.p5_sr) #PASSES!!!! p = .16
testOutliers(ss.p5_sr)
testZeroInflation(ss.p5_sr) #looks good p =.5

# Plotting standardized residuals against predictors
plotResiduals(ss.p5_sr, ss_total.all$Treat_Type, xlab = "Treatment Type", main=NULL)  

plotResiduals(ss.p5_sr, ss_total.all$Site, xlab = "Site", main=NULL)

######### running emmeans for pairwise
emmeans(ss.p5, specs = pairwise ~ Treat_Type, adjust = 'Tukey', type = 'response')

# now just FallRx and Harvest are significant, SpringRX now has a p value of 0.0593



# ******************** don't add all these packages until the end - they mess with tidyverse and create a real problem for loading other data sets


# time to move on to plotting!!! -------------------

library(ggeffects)
library(ggiraphExtra)
library(ggiraph)
library(plyr)


ggpredict(ss.p5, terms = c('avgLD_l', 'Treat_Type'))|> plot() #this works but doesn't look great - using ggplot would be more customizable



















library(lmtest)


emmeans(ss.nb_all.factors3, specs = pairwise ~ Treat_Type, adjust = "Tukey", type = "response")
# infinite degrees of freedom means that this is a z test and not a t-test; z-confidence interval; how emmeans labels asymptotic tests; all glmers in emmeans are calculated this way




library(sjPlot)
library(sjlabelled)
library(sjmisc)

plot_model(ss.p5) #this is incidence rate ratios

plot_model(ss.p5, type = "diag") #random vs normal quantiles

plot_model(ss.p5, type = "re") #this plots random effects

plot_model(ss.p5, 
           type = 'pred', 
           terms = 'Treat_Type') #plot marginal effects (i might have done this wrong)


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

# 
# "#85D4E3" "#F4B5BD" "#9C964A" "#CDC08C"
# "#FAD77B"   
# 
# "#D8B70A" "#02401B" "#A2A475" "#81A88D"
# "#972D15"

# SS PIRI plot 1 -------------------
ss.plot1

plot_model(ss.p5, 
                       type = 'pred', 
                       terms = c('avgLD_l', 'Treat_Type'),
                       axis.title = c("Average Leaf Litter Depth (log transformed)", "Total Count of Pitch Pine"),
                       title = "Predicted Counts of Pitch Pine Seedlings <50cm",
           legend.title = "Treatment Type",
           line.size = 1,
           value.offset = 'Treat_Type',
           ci.lvl = NA,
           colors = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15")) 




# ss.plot1 + font_size(title = "Predicted Counts of Pitch Pine Seedlings <50cm",
#                      axis_title.x = "Average Leaf Litter Depth (log transformed)",
#                      axis_title.y = "Total Count of Pitch Pine (Pinus rigida)")






# SS Plot 2 -------------------
plot_model(ss.p5, type = 'pred', 
           terms = c('l.BA_HA', 'Treat_Type'),
           axis.title = c("Basal Area per Hectare (log transformed)", "Total Count of Pitch Pine"),
           title = "Predicted Counts of Pitch Pine Seedlings <50cm",
           legend.title = "Treatment Type",
           ci.lvl = NA,
           colors = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"))

#this graph is no longer relevant, as the model with BA fails to converge - not sure if I should try other models etc. 










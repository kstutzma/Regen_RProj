# Stutzman
# Scrub oak script
#  2023-12-11


#operating from the base that I've already run the base SS LS & SA scripts, from which it is possible to get this info


########### start with SS -------------------------
source("Scripts/SS_Import.R")

# filter for just Shrub Oak
ss_SO <- ss_merge %>% 
  filter(Species_Groups == "Shrub Oak") %>% 
  arrange(Treat_Type)


# now to make the explanatory variables factors
ss_SO$Treat_Type = factor(ss_SO$Treat_Type,
                            levels = unique(ss_SO $Treat_Type))
ss_SO$Region = factor(ss_SO$Region,
                        levels = unique(ss_SO$Region))
ss_SO$Site = factor(ss_SO$Site,
                      levels = unique(ss_SO$Site))


# now would like to import time since data set and join to ss data set -------------------
time_since <- read_csv("CleanData/Treat_Year_Data.csv")

ss_SO1 <- merge(ss_SO, time_since, by = "Site")
ss_SO1$l.TFT <- log(ss_SO1$Time_from_Treat)

# merge with info from Add_BA.R script, dataframe called prism_BA
ss_SO2 <- merge(ss_SO1, prism_BA, by = 'Plot_No')

# merge with info from Ground_Data.R script, dataframe called ground3
ss_SO.all <- merge(ss_SO2, ground3, by = "Plot_No")

rm(ss_SO1,
   ss_SO2)


# I should look at the distribution of scrub oak-------------------
ggplot(ss_SO.all, aes(x = Species_Groups, y=Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))

ggplot(ss_SO.all, aes(x=Total)) +
  geom_histogram(binwidth = 1) +
  facet_grid(rows = vars(Treat_Type))


# ran models using poisson distro, but the data is underdispersed. Reading up, I should try using a 'compois' (COM-Poisson) or 'genpois' (Generalized Poisson) distribution in the glmmTMB package. Lets start with genpois


so.ss_gp1 <- glmmTMB(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                     data = ss_SO.all,
                     family = genpois)
summary(so.ss_gp1) #AIC is 1862.7


# using DHARMa package to check model fit -------------------
so.ss1_sr <- simulateResiduals(fittedModel = so.ss_gp1, n = 1000, plot = TRUE) #passes the test

testDispersion(so.ss1_sr) 
#alternative = 'less') #the model is underdispersed
testOutliers(so.ss1_sr) #fine
testZeroInflation(so.ss1_sr) #fine

# some issues with residual quantiles. Going to drop items from the model and see what happens to the AIC, then re-test in DHARMa and see what happens to residual quantiles

so.ss_gp2 <- glmmTMB(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = genpois)
AIC(so.ss_gp2) #1850

so.ss_gp3 <- glmmTMB(Total~Treat_Type + avgLD_l + l.BA_HA + l.Mineral + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = genpois)
AIC(so.ss_gp3) #1848.7

library(lmtest)
lrtest(so.ss_gp2, so.ss_gp3) # p value is 0.37

so.ss_gp4 <- glmmTMB(Total~Treat_Type + avgLD_l + l.BA_HA + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = genpois)
AIC(so.ss_gp4) #1847

lrtest(so.ss_gp3, so.ss_gp4) # p = .5

so.ss_gp5 <- glmmTMB(Total~Treat_Type + avgLD_l + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = genpois)
AIC(so.ss_gp5) #1845.1

so.ss_gp6 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = genpois)
AIC(so.ss_gp6) #1845.8

lrtest(so.ss_gp5, so.ss_gp6) # p=.1, so not worth it to keep avgLD

so.ss6_sr <- simulateResiduals(fittedModel = so.ss_gp6, n = 1000, plot = TRUE) #passes the test

testDispersion(so.ss6_sr, alternative = 'less') # passes
testOutliers(so.ss6_sr) #fine
testZeroInflation(so.ss6_sr) #fine



so.ss5_sr <- simulateResiduals(fittedModel = so.ss_gp5, n = 1000, plot = TRUE) #still has issues with quantile predictions

testDispersion(so.ss5_sr) #passes
testOutliers(so.ss5_sr) #fine
testZeroInflation(so.ss5_sr) #fine

so.ss_cp7 <- glmmTMB(Total~Treat_Type + avgLD_l + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = compois)
AIC(so.ss_cp7) #1866.2; doesn't seem like a better fit but will look in DHARMa

so.ss7_sr <- simulateResiduals(fittedModel = so.ss_cp7, n = 1000, plot = TRUE) #gets rid of residual quantile issues

testDispersion(so.ss7_sr) # just barely passes
testDispersion(so.ss7_sr, alternative = 'less') #doesn't pass, model is still underdispersed
testDispersion(so.ss7_sr, alternative = 'greater')# passes
testOutliers(so.ss7_sr) #fine
testZeroInflation(so.ss7_sr) #fine




# trying best model with com pois distro
so.ss_cp8 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = compois)
AIC(so.ss_cp8) #1865.9

so.ss8_sr <- simulateResiduals(fittedModel = so.ss_cp8, n = 1000, plot = TRUE) 

testDispersion(so.ss8_sr) # just barely passes
testDispersion(so.ss8_sr, alternative = 'less') #doesn't pass, model is still underdispersed
testDispersion(so.ss8_sr, alternative = 'greater')# passes
testOutliers(so.ss8_sr) #fine
testZeroInflation(so.ss8_sr) 




# this is the best model -------------------
so.ss_gp6 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                     data = ss_SO.all,
                     family = genpois)
summary(so.ss_gp6)


rm(so.ss1_sr,
   so.ss5_sr,
   so.ss7_sr,
   so.ss8_sr,
   so.ss_cp8,
   so.ss_gp1,
   so.ss_gp2,
   so.ss_gp3,
   so.ss_gp4,
   so.ss_gp5,
   so.ss_gp7)


emmeans(so.ss_gp6,  specs = pairwise ~ Treat_Type, adjust = 'Tukey', type = 'response')
# no significant differences






































##################################### large seeding ------------------------------
source("Scripts/LS_Import.R")

# filter just for Shrub Oak
ls_SO <- ls_merge %>% 
  filter(Species_Groups == "Shrub Oak") %>% 
  arrange(Treat_Type)

# make explanatory variables factors -------------------
ls_SO$Treat_Type = factor(ls_SO$Treat_Type,
                            levels = unique(ls_SO$Treat_Type))
ls_SO$Region = factor(ls_SO$Region,
                        levels = unique(ls_SO$Region))
ls_SO$Site = factor(ls_SO$Site,
                      levels = unique(ls_SO$Site))



# adding time since
ls_SO2 <- merge(ls_SO, time_since, by = 'Site')
ls_SO2$l.TFT <- log(ls_SO2$Time_from_Treat)

# add BA
ls_SO3 <- merge(ls_SO2, prism_BA, by = "Plot_No")

# add ground cover
ls_SO.all <- merge(ls_SO3, ground3, byt = "Plot_No")

rm(ls_SO3, ls_SO2)





# ********  exploring total data using histogram -------------------
ggplot(ls_SO.all, aes(x=Total))+
  geom_histogram(binwidth = 2)+
  facet_grid(rows = vars(Treat_Type))

ggplot(ls_SO.all, aes(x=l.Total))+
  geom_histogram(binwidth = .2)+
  facet_grid(rows = vars(Treat_Type))

# ********  boxplots? -------------------
ggplot(ls_SO.all, aes(x = Species_Groups, y=Total))+
  geom_boxplot()+
  facet_wrap(vars(Treat_Type))


# start tyring models with a poisson distro -------------------
ls.so_all <- glmer(Total~Treat_Type + l.avgLD + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                   data = ls_SO.all,
                   family = poisson)
summary(ls.so_all) #failed to converge, AIC is 3398.7

ls.so1 <- glmer(Total~Treat_Type + l.avgLD + l.BA_HA + l.BA_piri + (1|Site/Plot_No) + offset(l.TFT),
                   data = ls_SO.all,
                   family = poisson)
summary(ls.so1) #failed to converge, AIC went up

ls.so2 <- glmer(Total~Treat_Type + (1|Site/Plot_No),
                data = ls_SO.all,
                family = poisson)
summary(ls.so2) #AIC 3406.9

ls.so3 <- glmer(Total~Treat_Type + l.Mineral + (1|Site/Plot_No),
                data = ls_SO.all,
                family = poisson)
summary(ls.so3) #AIC 3399.8

ls.so4 <- glmer(Total~Treat_Type + l.Mineral + l.BA_HA + (1|Site/Plot_No),
                data = ls_SO.all,
                family = poisson)
summary(ls.so4)
lrtest(ls.so4, ls.so3) # p is .2

#seems to be best model
ls.so5 <- glmer(Total~Treat_Type + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                data = ls_SO.all,
                family = poisson)
summary(ls.so5) #AIC 3399.8

lrtest(ls.so5, ls.so3) # gives it the same p-value (i think) as all other offset comps

rm(ls.so1, ls.so2, ls.so4, ls.so_all)




ls.so5_sr <- simulateResiduals(fittedModel = ls.so5, n = 1000, plot = TRUE)

ls.so3_sr <- simulateResiduals(fittedModel = ls.so3, n = 1000, plot = TRUE)
#neither are great, 5 seems to be better

testDispersion(ls.so5_sr) #again low p value
testDispersion(ls.so5_sr, alternative = 'less') # underdispersion
testDispersion(ls.so5_sr, alternative = 'greater')

testOutliers(ls.so5_sr) #good
testZeroInflation(ls.so5_sr) # ok - seeing zero inflation!

ls.so_zi1 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                     data = ls_SO.all,
                     ziformula = ~(1|Site/Plot_No),
                     family = poisson)
summary(ls.so_zi1)
#not working - will have to trouble shoot it
fixef(ls.so_zi1)

ls_SO.all$l.Total <- log(ls_SO.all$Total + 1)



ls.so_zi1.sr <- simulateResiduals(ls.so_zi1, n = 1000, plot = T)
testDispersion(ls.so_zi1.sr)


ls.so_nb5 <- glmer.nb(Total~Treat_Type + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                data = ls_SO.all)
summary(ls.so_nb5)

ls.so_nb5.sr <- simulateResiduals(fittedModel = ls.so_nb5, n = 1000, plot = TRUE)
testZeroInflation(ls.so_nb5.sr)
# this does not solve the issue


# I'm guessing that the models are again underdispersed and so I'll try to fix using genpois and compois distributions

ls.so_all <- glmmTMB(Total~Treat_Type + avgLD_l + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                   data = ls_SO.all,
                   family = genpois)
AIC(ls.so_all) #3286.5

#would like to see how this model looks in DHARMa tests before starting to drop variables

ls.soa_sr <- simulateResiduals(fittedModel = ls.so_all, n = 1000, plot = TRUE)
#Quantile deviations not looking good

testDispersion(ls.soa_sr, alternative = 'greater')
#still underdispersed

#going to look at most simple model and see if this changes

ls.so_0 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                     data = ls_SO.all,
                     family = genpois)
AIC(ls.so_0) #3296.3 has risen

ls.so0_sr <- simulateResiduals(fittedModel = ls.so_0, n = 1000, plot = TRUE)
testDispersion(ls.so0_sr, alternative = 'greater')
#still underdispersed

#Try compois? Then maybe think about log transform and ANOVA - there are a lot of shrub oak in this category

ls.so_cp0 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                   data = ls_SO.all,
                   family = compois)
AIC(ls.so_cp0) #3315.6

ls.so.cp0_sr <- simulateResiduals(fittedModel = ls.so_0, n = 1000, plot = TRUE)

testDispersion(ls.so.cp0_sr)
testDispersion(ls.so.cp0_sr, alternative = 'less')
testDispersion(ls.so.cp0_sr, alternative = 'greater')
#Still under dispersed




# trying something new -------------------
library(lme4)

#start with a log transformation on total - maybe think about square root as well? or other ways to transform the data

ls_SO.all$l.Total <- log(ls_SO.all$Total+1)

ls_lmm1 <- lmer(l.Total~Treat_Type + (1|Site),
                data = ls_SO.all)
summary(ls_lmm1)
# this won't let me use Plot Number as a random effect within Site

ls.lmm1_sr <- simulateResiduals(ls_lmm1, plot = TRUE) #not a good model
testDispersion(ls.lmm1_sr)
testZeroInflation(ls.lmm1_sr) #yes, zero inflated

# I don't know what to do

ls_zi1 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                  data = ls_SO.all,
                  ziformula=~1,
                  family = genpois)
summary(ls_zi1) #AIC 3274

ls.zi1_sr <- simulateResiduals(ls_zi1, n = 1000, plot = TRUE) #just barely passes; one category has significant deviance from uniformity; levene test for homogeneity of variance is significant

testDispersion(ls.zi1_sr)
testZeroInflation(ls.zi1_sr)
testOutliers(ls.zi1_sr)
#passes all these tests. Should look into what the above failures mean


ls_zi2 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                  data = ls_SO.all,
                  ziformula=~1,
                  family = compois)
summary(ls_zi2) #AIC 3283.6

ls.zi2_sr <- simulateResiduals(ls_zi2, n = 1000, plot = TRUE) #worse, same issues as above

testDispersion(ls.zi2_sr) #fails
testZeroInflation(ls.zi2_sr) #passes
testOutliers(ls.zi2_sr) #passes


ls_zi3 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                  data = ls_SO.all,
                  ziformula=~Region,
                  family = genpois)
AIC(ls_zi3) #AIC 3269.4

ls.zi3_sr <- simulateResiduals(ls_zi3, n = 1000, plot = TRUE) #just barely passes; one category has significant deviance from uniformity; levene test for homogeneity of variance is significant

testDispersion(ls.zi3_sr, alternative = 'less') #still underdispersed
testZeroInflation(ls.zi3_sr) #fine
testOutliers(ls.zi3_sr) #fine


ls_zi4 <- glmmTMB(Total~Treat_Type + (1|Site/Plot_No),
                  data = ls_SO.all,
                  ziformula=~Region,
                  family = nbinom1)
AIC(ls_zi4) #AIC 3269.4

ls.zi4_sr <- simulateResiduals(ls_zi4, n = 1000, plot = TRUE) #just barely passes; one category has significant deviance from uniformity; levene test for homogeneity of variance is significant

testDispersion(ls.zi4_sr, alternative = 'less') #still underdispersed
testZeroInflation(ls.zi4_sr) #fine
testOutliers(ls.zi4_sr)

# I think I am officially stumped for the moment. It appears that the data for shrub oak large seedlings are both under dispersed and zero inflated. Residuals are heteroskedastic. I'm not sure where to go next. I'll probably have to bug Maria - maybe I can just send her an email?


# standard Pearson residuals
plot(ls_zi3, resid(., type = "pearson") ~ fitted(.) , abline = 0)

# DHARMa residuals
plot(simulateResiduals(ls_zi3))

#maybe adding predictors will help?

ls_zi5 <- glmmTMB(Total~Treat_Type + avgLD_l + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                  data = ls_SO.all,
                  ziformula=~Region,
                  family = genpois)
AIC(ls_zi5) # 3272.2


ls.zi5_sr <- simulateResiduals(ls_zi5, n = 1000, plot = TRUE) #looks a lot better ... but

testDispersion(ls.zi5_sr, alternative = 'less') #under dispersed
testZeroInflation(ls.zi5_sr) #fine
testOutliers(ls.zi5_sr) #fine


ls_zi6 <- glmmTMB(Total~Treat_Type + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                  data = ls_SO.all,
                  ziformula=~Region,
                  family = compois)
AIC(ls_zi6) #3268.4

ls.zi6_sr <- simulateResiduals(ls_zi6, n = 1000, plot = TRUE) 

testDispersion(ls.zi6_sr, alternative = 'less') #under dispersed
testZeroInflation(ls.zi6_sr) #fine
testOutliers(ls.zi6_sr)

# I don't think any of these many models have solved the under dispersion problem, unlike in the SS data. 


library(lmtest)
lrtest(ls_zi5, ls_zi6)



ls_gp6 <- glmmTMB(Total~Treat_Type + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                  data = ls_SO.all,
                  family = genpois)
AIC(ls_gp6)

ls.gp6_sr <- simulateResiduals(ls_gp6, n=1000, plot = TRUE)

testDispersion(ls.gp6_sr, alternative = 'less') #fails
testZeroInflation(ls.gp6_sr) #passes
testOutliers(ls.gp6_sr) #passes

#what to do as data is still underdispersed?
emmeans(ls_gp6, specs = pairwise~Treat_Type, adjust = 'Tukey', type = 'response')
# this looks significant - but with model under dispersion, I'm not sure if it is believable



# this is worse
ls_cp6 <- glmmTMB(Total~Treat_Type + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                  data = ls_SO.all,
                  family = compois)
AIC(ls_cp6)

ls.cp6_sr <- simulateResiduals(ls_cp6, n=1000, plot = TRUE)

testDispersion(ls.cp6_sr, alternative = 'less') #fails
testZeroInflation(ls.cp6_sr) #fails
testOutliers(ls.cp6_sr) #passes



#maybe see what a gamma distribution does? doesn't run. I am officially stuck. Need to ask for help. Moving on for now
















# sapling data - 
source("Scripts/SA_Import.R")

# filter just for Shrub Oak
sa_SO <- sapling_merge %>% 
  filter(Species_Groups == "Shrub Oak") %>% 
  arrange(Treat_Type)

# make explanatory variables factors -------------------
sa_SO$Treat_Type = factor(sa_SO$Treat_Type,
                          levels = unique(sa_SO$Treat_Type))
sa_SO$Region = factor(sa_SO$Region,
                      levels = unique(sa_SO$Region))
sa_SO$Site = factor(sa_SO$Site,
                    levels = unique(sa_SO$Site))



# adding time since
sa_SO2 <- merge(sa_SO, time_since, by = 'Site')
sa_SO2$l.TFT <- log(sa_SO2$Time_from_Treat)

# add BA
sa_SO3 <- merge(sa_SO2, prism_BA, by = "Plot_No")

# add ground cover
sa.so_alldata <- merge(sa_SO3, ground3, byt = "Plot_No")

rm(sa_SO3, sa_SO2)


sa.so_zi1 <- glmmTMB(Total_Tally ~ Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
                     data = sa.so_alldata,
                     ziformula=~1,
                     family = poisson)
AIC(sa.so_zi1) #307.9

sa.soz1_sr <- simulateResiduals(sa.so_zi1, n = 1000, plot = TRUE)
testDispersion(sa.soz1_sr) #passes
testOutliers(sa.soz1_sr) #passes
testZeroInflation(sa.soz1_sr) #passes


sa.so_p1 <- glmmTMB(Total_Tally ~ Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
                     data = sa.so_alldata,
                     family = poisson)
AIC(sa.so_p1)

sa.so1_sr <- simulateResiduals(sa.so_p1, n=1000, plot = T)
testZeroInflation(sa.so1_sr) #passes, guess we don't need to use a zero inflated model
testDispersion(sa.so1_sr)

sa.so_all <- glmmTMB(Total_Tally ~ Treat_Type + l.BA_HA + avgLD_l + (1|Site/Plot_No) + offset(l.TFT),
                     data = sa.so_alldata,
                     family = poisson)
AIC(sa.so_all) #309

# best model -------------------
sa.so_p2 <- glmmTMB(Total_Tally ~ Treat_Type + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                     data = sa.so_alldata,
                     family = poisson)
AIC(sa.so_p2) #307.8, passes DHARMa test

lrtest(sa.so_all, sa.so_p2) # p = .4

sa.so_p1 <- glmmTMB(Total_Tally ~ Treat_Type + (1|Site/Plot_No) + offset(l.TFT),
                    data = sa.so_alldata,
                    family = poisson)
AIC(sa.so_p1) #305.9, doesn't pass DHARMa test

sa.so_p4 <- glmmTMB(Total_Tally ~ Treat_Type + (1|Site/Plot_No),
                    data = sa.so_alldata,
                    family = poisson)
AIC(sa.so_p4) #doesn't pass dharma test

lrtest(sa.so_p3, sa.so_p4) #offset is valuable







sa.so1_sr <- simulateResiduals(sa.so_p1, n=1000, plot = T) #combined adjusted quantile test is significant
testZeroInflation(sa.so1_sr) #passes
testDispersion(sa.so1_sr) #passes
testOutliers(sa.so1_sr) #passes




sa.so2_sr <- simulateResiduals(sa.so_p2, n=1000, plot = T) #combined adjusted quantile test is significant
testZeroInflation(sa.so2_sr) #passes
testDispersion(sa.so2_sr) #passes
testOutliers(sa.so2_sr) #passes

emmeans(sa.so_p2, specs = pairwise ~ Treat_Type, adjust = 'Tukey', type = 'response')
# no differences



















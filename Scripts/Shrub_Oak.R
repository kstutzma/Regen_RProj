# Stutzman
# Scrub oak script
#  2023-12-11


#operating from th ebase that I've already run the base SS LS & SA scripts, from which it is possible to get this info


########### start with SS -------------------------

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








library(lmtest)
# still a lot of zeros, so start with a poisson

ss.so_all <- glmer(Total~Treat_Type + l.avgLD + l.BA_HA + l.BA_piri + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                   data = ss_SO.all,
                   family = poisson)
summary(ss.so_all) #failed to converge, AIC of 1897.4

ss.so_1 <- glmer(Total~Treat_Type + l.avgLD + l.BA_HA + l.BA_piri + (1|Site/Plot_No) + offset(l.TFT),
                   data = ss_SO.all,
                   family = poisson)
summary(ss.so_1) #failed to converge, AIC is 1895.6

ss.so_2 <- glmer(Total~Treat_Type + l.avgLD + l.BA_HA + (1|Site/Plot_No) + offset(l.TFT),
                 data = ss_SO.all,
                 family = poisson)
summary(ss.so_2) #failed to converge, AIC is 1894.4

ss.so_3 <- glmer(Total~Treat_Type + l.avgLD + l.BA_HA + (1|Site/Plot_No),
                 data = ss_SO.all,
                 family = poisson)
summary(ss.so_3) #failed to converge, AIC is 1886.1

ss.so_4 <- glmer(Total~Treat_Type + l.BA_HA + (1|Site/Plot_No),
                 data = ss_SO.all,
                 family = poisson)
summary(ss.so_4) #finally converged, AIC is 1887


######## best model so far
ss.so_5 <- glmer(Total~Treat_Type + (1|Site/Plot_No),
                 data = ss_SO.all,
                 family = poisson)
summary(ss.so_5) #AIC is 1885.3

lrtest(ss.so_4, ss.so_5) # p = .89, so l.BA_HA not valuable

ss.so_6 <- glmer(Total~Treat_Type + l.Mineral + (1|Site/Plot_No),
                 data = ss_SO.all,
                 family = poisson)
summary(ss.so_6) #1885.6

lrtest(ss.so_6, ss.so_5) # p is .19, so l.Min no valuable

rm(ss.so_1, ss.so_2, ss.so_3, ss.so_4, ss.so_6, ss.so_7)





######## best model so far
ss.so_5 <- glmer(Total~Treat_Type + (1|Site/Plot_No),
                 data = ss_SO.all,
                 family = poisson)
summary(ss.so_5) #AIC is 1885.3

# using DHARMa package to check model fit -------------------
ss.so3_sr <- simulateResiduals(fittedModel = ss.so_5, n = 1000, plot = TRUE) #passes the test

testDispersion(ss.so3_sr) #this is significant, what does it mean?
testOutliers(ss.so3_sr) #fine
testZeroInflation(ss.so3_sr) #fine


emmeans(ss.so_5, specs = pairwise ~ Treat_Type, type = "response", adjust = "Tukey")
#fall rx vs harvest is different


































##################################### large seeding ------------------------------

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

ggplot(ls_SO.all, aes(x=l.Total))+
  geom_histogram(binwidth = .5)+
  facet_grid(rows = vars(Treat_Type))
#when log transformed (all together) it looks normal with a bunch of zeros. Not sure what distro to use

ls.so_lm1 <- lmm



ggplot(ls_SO.all, aes(x=Total))+
  geom_histogram(binwidth = 2)+
  facet_grid(rows = vars(Treat_Type))









ls.so_zi1.sr <- simulateResiduals(ls.so_zi1, n = 1000, plot = T)
testDispersion(ls.so_zi1.sr)


ls.so_nb5 <- glmer.nb(Total~Treat_Type + l.Mineral + (1|Site/Plot_No) + offset(l.TFT),
                data = ls_SO.all)
summary(ls.so_nb5)

ls.so_nb5.sr <- simulateResiduals(fittedModel = ls.so_nb5, n = 1000, plot = TRUE)
testZeroInflation(ls.so_nb5.sr)
# this does not solve the issue













